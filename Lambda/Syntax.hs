{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FunctionalDependencies #-}
{- |
Module      :  Lambda.Syntax
Description :  AST definition.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Lambda.Syntax where

import Control.Lens
import Control.Applicative
import Text.Parsec (SourcePos)
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
    deriving (Eq, Show)

-- Expression.
data ExprF a expr = EVar a Name
                  | ELit a Literal
                  | EAbs a Name expr
                  | EApp a expr expr
                  | ELet a Name expr expr
    deriving (Eq, Show, Functor, Foldable, Traversable)

dataOf :: Lens' (ExprF a e) a
dataOf f (EVar d n) = EVar <$> (f d) ?? n
dataOf f (ELit d l) = ELit <$> (f d) ?? l
dataOf f (EAbs d n e) = EAbs <$> (f d) ?? n ?? e
dataOf f (EApp d e1 e2) = EApp <$> (f d) ?? e1 ?? e2
dataOf f (ELet d n e1 e2) = ELet <$> (f d) ?? n ?? e1 ?? e2

unfix :: Lens' (Fix f) (f (Fix f))
unfix f (Fix x) = Fix <$> (f x)

-- Constructors that wrap the expression in a Fix.
fEVar :: a -> Name -> Fix (ExprF a)
fEVar d n = Fix $ EVar d n

fELit :: a -> Literal -> Fix (ExprF a)
fELit d lit = Fix $ ELit d lit

fEAbs :: a -> Name -> Fix (ExprF a) -> Fix (ExprF a)
fEAbs d n e = Fix $ EAbs d n e

fEApp :: a -> Fix (ExprF a) -> Fix (ExprF a) -> Fix (ExprF a)
fEApp d e1 e2 = Fix $ EApp d e1 e2

fELet :: a -> Name -> Fix (ExprF a) -> Fix (ExprF a) -> Fix (ExprF a)
fELet d n e1 e2 = Fix $ ELet d n e1 e2

-- Lenses.
class HasPos t where
    posOf :: Lens' t SourcePos

class HasType t where
    typeOf :: Lens' t Type

-- Expression with source position.
type PExprF = ExprF SourcePos
type PExpr = Fix PExprF

instance HasPos PExpr where
    posOf = unfix . dataOf

-- Typed, positioned expression.
type TPExprF = ExprF (SourcePos, Type)
type TPExpr = Fix TPExprF

instance HasPos TPExpr where
    posOf = unfix . dataOf . _1

instance HasType TPExpr where
    typeOf = unfix . dataOf . _2
