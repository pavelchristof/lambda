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
data UExpr' expr = Var Name
                 | Lit Literal
                 | Abs Name expr
                 | App expr expr
                 | Let Name expr expr
    deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type UExpr = Fix UExpr'

uVar :: Name -> UExpr
uVar = Fix . Var

uLit :: Literal -> UExpr
uLit = Fix . Lit

uAbs :: Name -> UExpr -> UExpr
uAbs n e = Fix $ Abs n e

uApp :: UExpr -> UExpr -> UExpr
uApp e1 e2 = Fix $ App e1 e2

uLet :: Name -> UExpr -> UExpr -> UExpr
uLet n e1 e2 = Fix $ Let n e1 e2

-- Typed expression.
data TExpr' expr = TExpr Type (UExpr' expr)
    deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type TExpr = Fix TExpr'
