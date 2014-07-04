{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
import Data.Scientific (Scientific)
import Data.Text.Lazy (Text)
import Data.Foldable
import Data.Traversable
import Data.Functor.Foldable (Fix(..))

import Lambda.Name
import Lambda.Type
import Lambda.SourceLoc

-- | Literal.
data Literal = LitChar Char
             | LitString Text
             | LitInteger Integer
             | LitReal Scientific
             | LitEmptyList
    deriving (Eq, Show)

-- | A pattern.
data Pattern = Decons Name [Located (Maybe Name)]
             | Wildcard (Maybe Name)
    deriving (Eq, Show)

-- | Expression.
data ExprF a expr = EVar a Name
                  | ELit a Literal
                  | EAbs a Name expr
                  | EApp a expr expr
                  | ELet a Name expr expr
                  | ECase a expr [(Located Pattern, expr)]
    deriving (Eq, Show, Functor, Foldable, Traversable)

dataOf :: Lens' (ExprF a e) a
dataOf f (EVar d n) = EVar <$> (f d) ?? n
dataOf f (ELit d l) = ELit <$> (f d) ?? l
dataOf f (EAbs d n e) = EAbs <$> (f d) ?? n ?? e
dataOf f (EApp d e1 e2) = EApp <$> (f d) ?? e1 ?? e2
dataOf f (ELet d n e1 e2) = ELet <$> (f d) ?? n ?? e1 ?? e2
dataOf f (ECase d e p) = ECase <$> (f d) ?? e ?? p

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

fECase :: a -> Fix (ExprF a) -> [(Located Pattern, Fix (ExprF a))] -> Fix (ExprF a)
fECase d e p = Fix $ ECase d e p

-- | Lenses.
class HasType t where
    typeOf :: Lens' t Type

-- | Expression with source position.
type LExprF = ExprF SourceRange
type LExpr = Fix LExprF

instance HasRange LExpr where
    srcRange = unfix . dataOf

-- | Typed, positioned expression.
type TLExprF = ExprF (SourceRange, Type)
type TLExpr = Fix TLExprF

instance HasRange TLExpr where
    srcRange = unfix . dataOf . _1

instance HasType TLExpr where
    typeOf = unfix . dataOf . _2

-- | Constructor definition.
data ConsDef = ConsDef Name [Type]
    deriving (Eq, Show)

-- | Top level declaration.
data Decl expr = DAssign Name expr
               | DData Name [Located ConsDef]
    deriving (Eq, Show, Functor, Foldable, Traversable)
