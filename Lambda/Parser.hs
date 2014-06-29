{- |
Module      :  Lambda.Parser
Description :  Parser.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Lambda.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative hiding ((<|>), many)
import Data.List (foldl')
import Data.Text (Text, pack)
import Data.Functor.Foldable
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as T
import qualified Data.Text as Text

import Lambda.Name
import Lambda.Syntax
import Lambda.Located

lambdaDef = T.LanguageDef
    { T.commentStart    = "{-"
    , T.commentEnd      = "-}"
    , T.commentLine     = "--"
    , T.nestedComments  = True
    , T.identStart      = letter
    , T.identLetter     = alphaNum <|> oneOf "_'"
    , T.opStart         = T.opLetter lambdaDef
    , T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , T.reservedOpNames = ["="]
    , T.reservedNames   = ["let", "in"]
    , T.caseSensitive   = True
    }

lexer = T.makeTokenParser lambdaDef

name :: Parser Name
name = fromString <$> T.identifier lexer

opName :: Parser Name
opName = fromString <$> T.operator lexer

located :: Parser a -> Parser (Located a)
located = (Located <$> getPosition <*>)

variable :: Parser UExpr
variable = uEVar <$> name
                 <?> "a variable"

abstraction :: Parser UExpr
abstraction = uEAbs <$ T.symbol lexer "λ" 
                    <*> name
                    <* T.dot lexer
                    <*> expr
                    <?> "an abstraction"

application :: Parser UExpr
application = foldl' uEApp <$> simpleExpr <*> many1 simpleExpr <?> "an application"

operator :: Parser UExpr
operator = uEApp <$> (flip uEApp <$> simpleExpr <*> (uEVar <$> opName)) <*> simpleExpr

letBinding :: Parser UExpr
letBinding = uELet <$ T.reserved lexer "let"
                   <*> name
                   <* T.reservedOp lexer "="
                   <*> expr
                   <* T.reserved lexer "in"
                   <*> expr
                   <?> "a let binding"

literal :: Parser UExpr
literal = uELit <$> literal'
                <?> "a literal"
    where
        literal' =   (LitChar <$> T.charLiteral lexer)
                 <|> (LitString . pack <$> T.stringLiteral lexer)
                 <|> (LitInteger <$> T.integer lexer)
                 <|> (LitDouble <$> T.float lexer)

simpleExpr :: Parser UExpr
simpleExpr =    T.parens lexer (uEVar <$> opName <|> expr)
            <|> literal
            <|> variable
            <?> "a variable, literal or a parenthesed expression"

expr :: Parser UExpr
expr =   letBinding 
     <|> abstraction
     <|> try application
     <|> try operator
     <|> simpleExpr
     <?> "an expression"

program :: Parser [UExpr]
program = many (expr <* T.semi lexer)
