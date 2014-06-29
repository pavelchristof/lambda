{- |
Module      :  Lambda.Type
Description :  Type.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Lambda.Type where

import Control.Applicative
import Control.Lens
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lambda.Name

-- A type. So far only types of kind * are supported.
data Type = TVar Name
          | TFun Type Type
          | TList Type
    deriving (Eq, Ord, Show, Read)

tChar :: Type
tChar = TVar $ Name "Char"

tInt :: Type
tInt = TVar $ Name "Int"

tDouble :: Type
tDouble = TVar $ Name "Double"

-- An universally quantified type.
data QuantType = QuantType (Set Name) Type
    deriving (Eq, Show, Read)

-- Structures containing free type variables.
class HasFreeTVars t where
    freeTVars :: Traversal t t Name Type

instance HasFreeTVars Type where
    freeTVars f (TVar n) = f n
    freeTVars f (TFun t1 t2) = TFun <$> freeTVars f t1 <*> freeTVars f t2
    freeTVars f (TList t) = TList <$> freeTVars f t

instance HasFreeTVars QuantType where
    freeTVars f (QuantType q t) = QuantType q <$> freeTVars f' t
        where f' name
                  | Set.member name q = pure . TVar $ name
                  | otherwise         = f name

instance HasFreeTVars [Type] where
    freeTVars f = traverse (freeTVars f)

-- Creates a set of all free type variables.
setOfFreeTVars :: HasFreeTVars t => t -> Set Name
setOfFreeTVars = Set.fromList . (toListOf (freeTVars . (fmap TVar .)))

-- Substitutes free type variables.
substFreeTVars :: HasFreeTVars t => Map Name Type -> t -> t
substFreeTVars dict = over freeTVars f
    where f name = case Map.lookup name dict of
                        Just t -> t
                        Nothing -> TVar name
