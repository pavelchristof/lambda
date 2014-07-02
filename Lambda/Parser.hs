{- |
Module      :  Lambda.Parser
Description :  Parser.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Lambda.Parser
    ( program
    ) where

import Control.Lens ((^.))
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
    , T.reservedOpNames = ["=", "[", "]", ","]
    , T.reservedNames   = ["let", "rec", "in", "True", "False"]
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
application = foldl' <$> (fEApp <$> getPosition) <*> atom <*> many1 atom <?> "an application"

opApplication :: Parser PExpr
opApplication = fEApp <$> getPosition 
                      <*> (flip <$> (fEApp <$> getPosition) <*> atom <*> opVariable) 
                      <*> expr
                      <?> "an operator application"

letExpr :: Parser PExpr
letExpr = fELet <$> getPosition
                <* T.reserved lexer "let"
                <*> (name <|> opName)
                <* T.reservedOp lexer "="
                <*> expr
                <* T.reserved lexer "in"
                <*> expr
                <?> "a let binding"

letRecExpr :: Parser PExpr
letRecExpr = do
    pos <- getPosition
    T.reserved lexer "let rec"
    n <- name <|> opName
    T.reservedOp lexer "="
    e1 <- expr
    T.reserved lexer "in"
    e2 <- expr
    return (fELet pos n (fEFix pos n e1) e2)

unit :: Parser PExpr
unit = fELit <$> getPosition
             <*> pure LitUnit
             <?> "an unit"

literal :: Parser PExpr
literal = fELit <$> getPosition
                <*> literal'
                <?> "a literal"
    where
        literal' =   (LitChar <$> T.charLiteral lexer)
                 <|> (LitBool <$> (T.reserved lexer "True" *> pure True <|> T.reserved lexer "False" *> pure False))
                 <|> (LitString . pack <$> T.stringLiteral lexer)
                 <|> (intOrDouble <$> T.naturalOrFloat lexer)
        intOrDouble (Left i) = LitInteger i
        intOrDouble (Right f) = LitDouble f

list :: Parser PExpr
list = do
    T.reservedOp lexer "["
    exprs <- T.commaSep lexer expr
    T.reservedOp lexer "]"
    pos <- getPosition
    return $ foldr buildList (fELit pos LitEmptyList) exprs
    where
        buildList e l =
            let pos = e^.posOf
            in (fEApp pos (fEApp pos (fEVar pos ":") e) l)

atom :: Parser PExpr
atom =   T.parens lexer (opVariable <|> expr <|> unit)
     <|> list
     <|> literal
     <|> variable
     <?> "a variable, literal or a parenthesed expression"

expr :: Parser PExpr
expr =   abstraction
     <|> letRecExpr
     <|> letExpr
     <|> try opApplication
     <|> try application
     <|> atom
     <?> "an expression"

letStmt :: Parser (Stmt PExpr)
letStmt = SLet <$> getPosition
               <* T.reserved lexer "let"
               <*> (name <|> opName)
               <* T.reservedOp lexer "="
               <*> expr

letRecStmt :: Parser (Stmt PExpr)
letRecStmt = do
    pos <- getPosition
    T.reserved lexer "let rec"
    n <- name <|> opName
    T.reservedOp lexer "="
    e <- expr
    return (SLet pos n (fEFix pos n e))

eval :: Parser (Stmt PExpr)
eval = SEval <$> getPosition
             <*> expr

stmt :: Parser (Stmt PExpr)
stmt =   try eval
     <|> letRecStmt
     <|> letStmt

program :: Parser [Stmt PExpr]
program = T.whiteSpace lexer *> many (stmt <* T.semi lexer)
