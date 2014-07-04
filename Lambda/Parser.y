-----------------------------------------------------------------------------
-- (c) Paweł Nowak, 2014
--
-- Lambda parser.
--
----------------------------------------------------------------------------- 

{

module Lambda.Parser
    ( parse
    ) where

import Control.Lens
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Text.Lazy (Text)
import Data.Scientific (Scientific)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Lambda.Name
import Lambda.Type
import Lambda.Lexer
import Lambda.Syntax
import Lambda.SourceLoc

}

%name parser
%tokentype { Located Token }
%monad { Parser }
%lexer { lexerWrapped } { L _ TEOF }
%error { parseError }

%token 
    let             { L _ TLet }
    in              { L _ TIn }
    case            { L _ TCase }
    of              { L _ TOf }
    data            { L _ TData }

    lambda          { L _ TLambda }
    '='             { L _ TAssign }
    '->'            { L _ TArrow }
    ','             { L _ TComma }
    ';'             { L _ TSemi }
    '|'             { L _ TPipe }
    '('             { L _ TOParen }
    ')'             { L _ TCParen }
    '['             { L _ TOBracket }
    ']'             { L _ TCBracket }
    '{'             { L _ TOBrace }
    '}'             { L _ TCBrace }
    '_'             { L _ TUnderscore }

    integer         { L _ (TInteger _) }
    real            { L _ (TReal _) }
    char            { L _ (TChar _) }
    string          { L _ (TString _) }

    lowid           { L _ (TLowerId _) }
    uppid           { L _ (TUpperId _) }
    symbol          { L _ (TSymbol _) }

%right in '->'
%left symbol

%%

decls :: { [Located (Decl LExpr)] }
decls : declsR                         { reverse $1 }

declsR :: { [Located (Decl LExpr)] }
declsR : {- empty -}                   { [] } 
       | declsR decl ';'               { $2 : $1 } 

decl :: { Located (Decl LExpr) }
decl : sureVar vars '=' expr           { DAssign <\$> $1 <*> gatherLoc (genAbs (reverse $2) $4) }
     | sureVar symbol sureVar '=' expr { DAssign <\$> fmap getName $2 <*> gatherLoc (genAbs (map (fmap Just) [$1, $3]) $5) }
     | data uppid '=' consList         { DData <\$ $1 <*> fmap getName $2 <* $3 <*> gatherLoc (reverse $4) }

consList :: { [Located ConsDef] }
consList : cons                        { [$1] }
         | consList '|' cons           { $3 : $1 }

cons :: { Located ConsDef }
cons : uppid typeList                  { ConsDef <\$> fmap getName $1 <*> pure (reverse $2) }

typeList :: { [Type] }
typeList : {- empty -}                 { [] }
         | typeList typeLit            { $2 : $1 }

typeLit :: { Type }
typeLit : uppid                        { TLit . getName . unLoc \$ $1 }
        | typeLit '->' typeLit         { TFun $1 $3 }
        | '[' typeLit ']'              { TList $2 }
        | '(' typeLit ')'              { $2 }
        | '(' ')'                      { tUnit }

expr :: { LExpr }
expr : lambda var vars '->' expr           { genAbs ($2:(reverse $3)) $5 }
     | let sureVar vars '=' expr in expr   { fELet (spanLoc $1 $7) (unLoc $2) (genAbs (reverse $3) $5) $7 }
     | case expr of '{' patCases '}'       { fECase (spanLoc $1 $6) $2 (reverse $5) }
     | expr atom                           { fEApp (spanLoc $1 $2) $1 $2 }
     | expr symbol expr                    { operApp $1 (fmap getName $2) $3 }
     | atom                                { $1 }

atom :: { LExpr }
atom : '(' symbol ')'                  { fEVar (getLoc $2) (getName \$ unLoc $2) }
     | '(' expr ')'                    { $2 }
     | '[' list ']'                    { genList $1 (reverse $2) $3 }
     | '(' ')'                         { fEVar (spanLoc $1 $2) "()" }
     | integer                         { fELit (getLoc $1) (LitInteger . getInteger . unLoc \$ $1) }
     | real                            { fELit (getLoc $1) (LitReal . getReal . unLoc \$ $1) }
     | char                            { fELit (getLoc $1) (LitChar . getCh . unLoc \$ $1) }
     | string                          { fELit (getLoc $1) (LitString . getText . unLoc \$ $1) }
     | lowid                           { fEVar (getLoc $1) (getName \$ unLoc $1) }
     | uppid                           { fEVar (getLoc $1) (getName \$ unLoc $1) } -- treat the constructor as a function

list :: { [LExpr] }
list : {- empty -}                     { [] }
     | expr                            { [$1] }
     | list ',' expr                   { $3 : $1 }

patCases :: { [(Located Pattern, LExpr)] }
patCases : patCase                     { [$1] }
         | patCases ';' patCase        { $3 : $1 }

patCase :: { (Located Pattern, LExpr) }
patCase : pattern '->' expr            { ($1, $3) }

-- A pattern can be a variable name or a constructor with a list of variable names.
pattern :: { Located Pattern }
pattern : var                          { Wildcard <\$> $1 }
        | uppid vars                   { Decons <\$> fmap getName $1 <*> gatherLoc (reverse $2) }
        | symbol vars                  { Decons <\$> fmap getName $1 <*> gatherLoc (reverse $2) }
        | '(' ')'                      { L (spanLoc $1 $2) (Decons "()" []) }

-- A list of variables used in a pattern.
vars :: { [Located (Maybe Name)] }
vars : {- empty -}                     { [] }
     | vars var                        { $2 : $1 }

var :: { Located (Maybe Name) }
var : sureVar                          { fmap Just $1 }
    | '_'                              { fmap (const Nothing) $1 }

sureVar :: { Located Name }
sureVar : lowid                        { fmap getName $1 }
        | '(' symbol ')'               { fmap getName $2 }

{

-- | Extracts a name from a token.
getName :: Token -> Name
getName (TLowerId n) = n
getName (TUpperId n) = n
getName (TSymbol n) = n
getName t = error $ "called getName on " ++ show t

-- | Extracts an integer from a token.
getInteger :: Token -> Integer
getInteger (TInteger i) = i
getInteger t = error $ "called getInteger on " ++ show t

-- | Extracts an real from a token.
getReal :: Token -> Scientific
getReal (TReal r) = r
getReal t = error $ "called getReal on " ++ show t

-- | Extracts a character from a token.
getCh :: Token -> Char
getCh (TChar c) = c
getCh t = error $ "called getCh on " ++ show t

-- | Extracts text from a token.
getText :: Token -> Text
getText (TString t) = t
getText t = error $ "called getText on " ++ show t

-- Applies an operator.
operApp :: LExpr -> Located Name -> LExpr -> LExpr
operApp e1 n e2 = app2
    where
        opVar = fEVar (getLoc n) (unLoc n)
        app1 = fEApp (spanLoc e1 n) opVar e1
        app2 = fEApp (spanLoc e1 e2) app1 e2

-- Generates a list by applying operator ':'.
genList :: Located Token -> [LExpr] -> Located Token -> LExpr
genList pre list post = foldr listCons emptyList list
    where
        listCons :: LExpr -> LExpr -> LExpr
        listCons e l = operApp e (L NoRange ":") l
        emptyList :: LExpr
        emptyList = fELit (getLoc post) LitEmptyList

-- Generates abstraction from a list of names.
genAbs :: [Located (Maybe Name)] -> LExpr -> LExpr
genAbs l e = foldr addAbs e l
    where 
        addAbs name expr = fEAbs (spanLoc name expr) (nameFromMb $ unLoc name) expr

-- Returns a reserved name if the maybe is nothing.
nameFromMb :: Maybe Name -> Name
nameFromMb (Just n) = n
nameFromMb Nothing = reservedName

-- | Parser state, contains lexer state.
data ParserState = ParserState
    { _lexerState :: LexerState
    }

-- | Happy does not work with template haskell..
lexerState :: Lens' ParserState LexerState
lexerState f (ParserState lexSt) = ParserState <$> f lexSt

-- | The Parser monad.
type Parser = StateT ParserState (Except String)

-- | Runs the given on a byte string.
runParser :: Parser a -> ByteString -> Either String a
runParser m bs = runExcept (evalStateT m st)
    where st = ParserState (initLexerState bs)

-- | Parses a lambda program.
parse :: ByteString -> Either String [Located (Decl LExpr)]
parse = runParser parser

-- | Calls the lexer.
lexerWrapped :: (Located Token -> Parser a) -> Parser a
lexerWrapped cont = zoom lexerState lexer >>= cont

-- | Reports a parser error.
parseError :: Located Token -> Parser a
parseError (L _ t) = do
    lexSt <- use lexerState
    throwError $ "Parse error before token " ++ show t 
        ++ " on line " ++ (show . line . location $ lexSt)
        ++ ", column " ++ (show . column . location $ lexSt)
        ++ "."

main :: IO ()
main = ByteString.getContents >>= print . parse

}
