{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{- |
Module      :  Lambda.Located
Description :  Functor containing source code position.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-} 

module Lambda.Located where

import Data.Foldable
import Data.Traversable
import Text.Parsec.Pos (SourcePos)

data Located a = Located SourcePos a
    deriving (Eq, Show, Functor, Foldable, Traversable)

location :: Located a -> SourcePos
location (Located loc _) = loc
