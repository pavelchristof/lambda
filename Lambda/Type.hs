{-# LANGUAGE TemplateHaskell, Rank2Types #-}
{- |
Module      :  Lambda.Type
Description :  Type definition.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Lambda.Type where

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lambda.Name

-- A type. So far only types of kind * are supported.
data Type = TVar Name
          | TLit Name
          | TFun Type Type
          | TList Type
    deriving (Eq, Ord, Show)

tUnit :: Type
tUnit = TLit $ Name "()"

tBool :: Type
tBool = TLit $ Name "Bool"

tChar :: Type
tChar = TLit $ Name "Char"

tInt :: Type
tInt = TLit $ Name "Int"

tDouble :: Type
tDouble = TLit $ Name "Double"

-- An universally quantified type.
data TypeScheme = TypeScheme (Set Name) Type
    deriving (Eq, Show)

-- Structures containing free type variables.
class HasFreeTVars t where
    freeTVars :: Traversal t t Name Type

instance HasFreeTVars Type where
    freeTVars f (TVar n) = f n
    freeTVars f t@(TLit _) = pure t
    freeTVars f (TFun t1 t2) = TFun <$> freeTVars f t1 <*> freeTVars f t2
    freeTVars f (TList t) = TList <$> freeTVars f t

instance HasFreeTVars TypeScheme where
    freeTVars f (TypeScheme q t) = TypeScheme q <$> freeTVars f' t
        where f' name
                  | Set.member name q = pure . TVar $ name
                  | otherwise         = f name

instance (Traversable t, HasFreeTVars a) => HasFreeTVars (t a) where
    freeTVars = traverse . freeTVars

-- Type substitution.
newtype TSubst = TSubst { _subst :: Map Name Type }
makeIso ''TSubst

instance Monoid TSubst where
    mempty = TSubst Map.empty
    s1 `mappend` s2 = TSubst $ _subst (apply s1 s2) `Map.union` _subst s1

instance HasFreeTVars TSubst where
    freeTVars = subst . freeTVars

-- Creates a set of all free type variables.
setOfFreeTVars :: HasFreeTVars t => t -> Set Name
setOfFreeTVars = Set.fromList . (toListOf (freeTVars . (fmap TVar .)))

-- Substitutes free type variables.
apply :: HasFreeTVars t => TSubst -> t -> t
apply (TSubst subst) = over freeTVars f
    where f name = case Map.lookup name subst of
                        Just t -> t
                        Nothing -> TVar name
