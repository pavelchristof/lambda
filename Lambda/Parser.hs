{-# LANGUAGE OverloadedStrings #-}
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
name = Name . pack <$> T.identifier lexer

opName :: Parser Name
opName = Name . pack <$> T.operator lexer

variable :: Parser UExpr
variable = uVar <$> name
                <?> "a variable"

abstraction :: Parser UExpr
abstraction = uAbs <$ T.symbol lexer "λ" 
                   <*> name 
                   <* T.dot lexer
                   <*> expr
                   <?> "an abstraction"

application :: Parser UExpr
application = foldl' uApp <$> simpleUExpr <*> many1 simpleUExpr <?> "an application"

operator :: Parser UExpr
operator = uApp <$> (flip uApp <$> simpleUExpr <*> (uVar <$> opName)) <*> simpleUExpr

letBinding :: Parser UExpr
letBinding = uLet <$ T.reserved lexer "let"
                  <*> name
                  <* T.reservedOp lexer "="
                  <*> expr
                  <* T.reserved lexer "in"
                  <*> expr
                  <?> "a let binding"

literal :: Parser UExpr
literal = uLit <$> literal'
               <?> "a literal"
    where
        literal' =   (LitChar <$> T.charLiteral lexer)
                 <|> (LitString . pack <$> T.stringLiteral lexer)
                 <|> (LitInteger <$> T.integer lexer)
                 <|> (LitDouble <$> T.float lexer)

simpleUExpr :: Parser UExpr
simpleUExpr =   T.parens lexer (uVar <$> opName <|> expr)
            <|> literal
            <|> variable
            <?> "a variable, literal or a parenthesed expression"

expr :: Parser UExpr
expr =   letBinding 
     <|> abstraction
     <|> try application
     <|> try operator
     <|> simpleUExpr
     <?> "an expression"

program :: Parser [UExpr]
program = many (expr <* T.semi lexer)
