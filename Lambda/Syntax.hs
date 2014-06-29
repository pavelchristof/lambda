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

import Text.Parsec (SourcePos)
import Data.Text (Text)
import Data.Foldable
import Data.Traversable
import Data.Functor.Foldable (Fix(..))

import Lambda.Name
import Lambda.Type
import Lambda.Located

data Literal = LitChar Char
             | LitString Text
             | LitInteger Integer
             | LitDouble Double
    deriving (Eq, Show)

-- Untyped expression.
data UExpr' expr = EVar Name
                 | ELit Literal
                 | EAbs Name expr
                 | EApp expr expr
                 | ELet Name expr expr
    deriving (Eq, Show, Functor, Foldable, Traversable)

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

-- Located expression.
data LUExpr' expr = LUExpr SourcePos (UExpr' expr)
    deriving (Eq, Show, Functor, Foldable, Traversable)

type LUExpr = Fix LUExpr'

luEVar :: SourcePos -> Name -> LUExpr
luEVar p n = Fix (LUExpr p (EVar n))

luELit :: SourcePos -> Literal -> LUExpr
luELit p lit = Fix (LUExpr p (ELit lit))

luEAbs :: SourcePos -> Name -> LUExpr -> LUExpr
luEAbs p n e = Fix (LUExpr p (EAbs n e))

luEApp :: SourcePos -> LUExpr -> LUExpr -> LUExpr
luEApp p e1 e2 = Fix (LUExpr p (EApp e1 e2))

luELet :: SourcePos -> Name -> LUExpr -> LUExpr -> LUExpr
luELet p n e1 e2 = Fix (LUExpr p (ELet n e1 e2))

-- Typed expression.
data TExpr' expr = TExpr Type (UExpr' expr)
    deriving (Eq, Show, Functor, Foldable, Traversable)

type TExpr = Fix TExpr'

tEVar :: Type -> Name -> TExpr
tEVar t n = Fix (TExpr t (EVar n))

tELit :: Type -> Literal -> TExpr
tELit t lit = Fix (TExpr t (ELit lit))

tEAbs :: Type -> Name -> TExpr -> TExpr
tEAbs t n e = Fix (TExpr t (EAbs n e))

tEApp :: Type -> TExpr -> TExpr -> TExpr
tEApp t e1 e2 = Fix (TExpr t (EApp e1 e2))

tELet :: Type -> Name -> TExpr -> TExpr -> TExpr
tELet t n e1 e2 = Fix (TExpr t (ELet n e1 e2))

typeOf :: TExpr -> Type
typeOf (Fix (TExpr t _)) = t

-- Located expressions.
-- type LUExpr' expr = Located (UExpr' expr)
-- type LUExpr = Fix LUExpr'
-- type LUExpr = Located UExpr
-- type LTExpr = Located TExpr
