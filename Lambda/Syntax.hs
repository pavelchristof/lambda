{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{- |
Module      :  Lambda.Syntax
Description :  
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Lambda.Syntax where

import Data.Text (Text)
import Data.Foldable
import Data.Traversable
import Data.Functor.Foldable (Fix(..))

import Lambda.Name
import Lambda.Type

data Literal = LitChar Char
             | LitString Text
             | LitInteger Integer
             | LitDouble Double
    deriving (Eq, Show, Read)

-- Untyped expression.
data UExpr' expr = EVar Name
                 | ELit Literal
                 | EAbs Name expr
                 | EApp expr expr
                 | ELet Name expr expr
    deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type UExpr = Fix UExpr'

uEVar :: Name -> UExpr
uEVar = Fix . EVar

uELit :: Literal -> UExpr
uELit = Fix . ELit

uEAbs :: Name -> UExpr -> UExpr
uEAbs n e = Fix $ EAbs n e

uEApp :: UExpr -> UExpr -> UExpr
uEApp e1 e2 = Fix $ EApp e1 e2

uELet :: Name -> UExpr -> UExpr -> UExpr
uELet n e1 e2 = Fix $ ELet n e1 e2

-- Typed expression.
data TExpr' expr = TExpr Type (UExpr' expr)
    deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type TExpr = Fix TExpr'
