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
import Data.String
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

variable :: Parser PExpr
variable = fEVar <$> getPosition
                 <*> name
                 <?> "a variable"

opVariable :: Parser PExpr
opVariable = fEVar <$> getPosition
                   <*> opName
                   <?> "an operator"

abstraction :: Parser PExpr
abstraction = fEAbs <$> getPosition 
                    <* T.symbol lexer "λ" 
                    <*> name
                    <* T.dot lexer
                    <*> expr
                    <?> "an abstraction"

application :: Parser PExpr
application = foldl' <$> (fEApp <$> getPosition) <*> simpleExpr <*> many1 simpleExpr <?> "an application"

operator :: Parser PExpr
operator = fEApp <$> getPosition 
                 <*> (flip <$> (fEApp <$> getPosition) <*> simpleExpr <*> opVariable) 
                 <*> simpleExpr
                 <?> "an operator"

letBinding :: Parser PExpr
letBinding = fELet <$> getPosition
                   <* T.reserved lexer "let"
                   <*> name
                   <* T.reservedOp lexer "="
                   <*> expr
                   <* T.reserved lexer "in"
                   <*> expr
                   <?> "a let binding"

literal :: Parser PExpr
literal = fELit <$> getPosition
                <*> literal'
                <?> "a literal"
    where
        literal' =   (LitChar <$> T.charLiteral lexer)
                 <|> (LitString . pack <$> T.stringLiteral lexer)
                 <|> (LitInteger <$> T.integer lexer)
                 <|> (LitDouble <$> T.float lexer)

simpleExpr :: Parser PExpr
simpleExpr =    T.parens lexer (opVariable <|> expr)
            <|> literal
            <|> variable
            <?> "a variable, literal or a parenthesed expression"

expr :: Parser PExpr
expr =   letBinding 
     <|> abstraction
     <|> try operator
     <|> try application
     <|> simpleExpr
     <?> "an expression"

program :: Parser [PExpr]
program = many (expr <* T.semi lexer)
