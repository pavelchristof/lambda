-----------------------------------------------------------------------------
-- (c) Paweł Nowak, 2014
--
-- Lambda lexer.
--
-----------------------------------------------------------------------------

{
{-# LANGUAGE FlexibleContexts #-}

module Lambda.Lexer
    ( Token(..)
    , LexerState
    , location
    , initLexerState
    , lexer
    ) where

import Control.Monad.Loops
import Control.Monad.Except
import Control.Monad.State
import Data.Word (Word8)
import Data.Scientific (Scientific)
import Data.Text.Lazy (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Internal as ByteString (w2c)

import Lambda.Name
import Lambda.SourceLoc
}

$lambda = λ
$digit  = 0-9

$lower  = [a-z \_]
$upper  = [A-Z]
$idchar = [$lower $upper $digit \']

$special = [\( \) \, \; \[ \] \{ \}]
$symchar = [\! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ \:]

-- Lexical analysis needs to be able to identify a constructor, for example:
-- case x of
--     y -> 5
-- Is y a constructor or a variable? 
--
-- We assume that:
--     - A constructor starts with an upper case letter or is an symbol.
--     - All variable names in patterns start with an lower case letter.
--     - For consistency, variable names introduced by a let expression and not a pattern matching start with a lower
--       case letter OR are a symbol.
@lowid  = $lower $idchar* -- variable or type variable
@uppid  = $upper $idchar* -- constructor or type
@symbol = $symchar+       -- operator or constructor, an operator cannot be mistaken for a variable name

-- Numbers. 
-- TODO: how to parse 2-2 as 2 - 2? it probably has to be done in the lexer
-- TODO: parse integrals as scientific too and convert to desired numeral type (when we have typeclasses - never).
@natural    = $digit+
@integral   = \-? @natural
@fractional = @integral \. @natural
@exponent   = [eE] [\- \+]? @natural
@scientific = @fractional @exponent?

lambda :-
-- White space and comments.
    $white+                                ;
    "--".*                                 ;
    "{-".*"-}"                             ;

-- Keywords.
    let                                    { const TLet }
    in                                     { const TIn }
    case                                   { const TCase }
    of                                     { const TOf }
    data                                   { const TData }

-- Reserved symbols.
    $lambda                                { const TLambda }
    \=                                     { const TAssign }
    \-\>                                   { const TArrow }
    \,                                     { const TComma }
    \;                                     { const TSemi }
    \|                                     { const TPipe }
    \(                                     { const TOParen }
    \)                                     { const TCParen }
    \[                                     { const TOBracket }
    \]                                     { const TCBracket }
    \{                                     { const TOBrace }
    \}                                     { const TCBrace }
    \_                                     { const TUnderscore }

-- Literals. TODO: escape characters inside strings and char literals. This will require adding state to the lexer.
-- This shoudn't change anything outside the lexer so lets just create a THUNK here and evaluate it when needed.
    @integral                              { TInteger . read . Text.unpack . Text.decodeUtf8 }
    @scientific                            { TReal . read . Text.unpack . Text.decodeUtf8 }
    \'.\'                                  { TChar . flip Text.index 1 . Text.decodeUtf8 }
    \".*"                                  { TString . Text.init . Text.tail . Text.decodeUtf8 }

-- Identifiers and operators.
@lowid                                     { TLowerId . Name . Text.decodeUtf8 }
@uppid                                     { TUpperId . Name . Text.decodeUtf8 }
@symbol                                    { TSymbol . Name . Text.decodeUtf8 }

{

data Token 
    = TLet        -- let
    | TIn         -- in
    | TCase       -- case
    | TOf         -- of
    | TData       -- data

    | TLambda     -- λ
    | TAssign     -- =
    | TArrow      -- ->
    | TComma      -- ,
    | TSemi       -- ;
    | TPipe       -- |
    | TOParen     -- (
    | TCParen     -- )
    | TOBracket   -- [
    | TCBracket   -- ]
    | TOBrace     -- {
    | TCBrace     -- }
    | TUnderscore -- _

    | TInteger Integer
    | TReal Scientific
    | TChar Char
    | TString Text

    | TLowerId Name
    | TUpperId Name
    | TSymbol Name

    | TEOF
    deriving (Eq, Show)

-- | Lexer state.
data LexerState = LexerState
    { location :: SourceLoc
    , prevChar :: Char
    , input :: ByteString
    }

-- | Creates an initial lexer state.
initLexerState :: ByteString -> LexerState
initLexerState input = LexerState (SourceLoc 1 1) '\n' input

-- | Alex input is just the lexer state.
type AlexInput = LexerState

-- | No pending bytes when lexing bytestring.
ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes = id

-- | Extracts the previous character.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = prevChar

-- | Extracts a byte from the stream.
-- TODO: proper unicode support? Does alex even allow that?
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (LexerState l _ i) | ByteString.null i = Nothing
                               | otherwise = let b = ByteString.head i
                                                 i' = ByteString.tail i
                                                 c = ByteString.w2c b
                                                 l' = locAfterChar c l
                                             in  l' `seq` i' `seq` Just (b, LexerState l' c i')

-- | Reads the next token.
lexer :: (MonadError String m, MonadState LexerState m) => m (Located Token)
lexer = do
    s <- get
    let l = location s
    case alexScan s 0 of
         AlexEOF -> return (L (SourceRange l l) TEOF)
         AlexError s' -> do
            put s'
            throwError $ "Lexical error at line " 
                      ++ (show . line . location $ s) 
                      ++ ", column " 
                      ++ (show . column . location $ s)
         AlexSkip s' len -> do
             put s'
             lexer
         AlexToken s' len act -> do
             put s'
             return (L (SourceRange l (location s')) $ act (ByteString.take (fromIntegral len) (input s)))

-- | Runs the lexer on the input stream and prints tokens, for debugging.
main :: IO ()
main = do
    s <- ByteString.getContents
    let f (L _ TEOF) = Nothing
        f t = Just t
    print $ runExcept (evalStateT (unfoldM (fmap f lexer)) $ initLexerState s)

}
