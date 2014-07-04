{-# LANGUAGE DeriveFunctor, OverlappingInstances #-}
{- |
Module      :  Lambda.SourceLoc
Description :  Source location and source ranges.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}

module Lambda.SourceLoc where

import Data.Monoid
import Data.Traversable
import Control.Lens
import Control.Applicative

-- | A location in the source code.
data SourceLoc = SourceLoc
    { line :: Int
    , column :: Int
    } deriving (Eq, Ord, Show)

-- | A range in the source code.
data SourceRange = SourceRange SourceLoc SourceLoc
                 | NoRange
    deriving (Eq, Show)

-- | Source ranges form a monoid .
instance Monoid SourceRange where
    mempty = NoRange
    mappend = unionRanges

-- | Some type with a source range.
data Located a = L
    { getLoc :: SourceRange
    , unLoc :: a
    } deriving (Eq, Show, Functor)

instance Applicative Located where
    pure = L NoRange
    (L l1 f) <*> (L l2 x) = L (l1 <> l2) (f x)

-- Structures that contain source ranges.
class HasRange t where
    srcRange :: Traversal' t SourceRange

instance HasRange SourceRange where
    srcRange = ($)

instance HasRange (Located a) where
    srcRange f (L l x) = L <$> f l ?? x

instance (Traversable t, HasRange a) => HasRange (t a) where
    srcRange f = traverse (srcRange f)

-- | Moves the source location past a character.
locAfterChar :: Char -> SourceLoc -> SourceLoc
locAfterChar char (SourceLoc l c) =
    case char of
         '\n' -> SourceLoc (l+1) 1
         '\t' -> SourceLoc l (((c `quot` 4) + 1)*4) -- sets the column to the nearest tab stop
         _    -> SourceLoc l (c+1)

-- | Constructs a source range that spans between two source locations.
sourceRange :: SourceLoc -> SourceLoc -> SourceRange
sourceRange a b = SourceRange (min a b) (max a b)

-- | Constructs a minimal source range that contains the given source ranges.
unionRanges :: SourceRange -> SourceRange -> SourceRange
unionRanges (SourceRange a b) (SourceRange a' b') = SourceRange (min a a') (max b b')
unionRanges NoRange b = b
unionRanges a NoRange = a

-- | Constructs a located from a value containing a range.
gatherLoc :: HasRange a => a -> Located a
gatherLoc x = L (foldOf srcRange x) x

-- | Constructs a range covering both a and b.
spanLoc :: (HasRange a, HasRange b) => a -> b -> SourceRange
spanLoc a b = (foldOf srcRange a) <> (foldOf srcRange b)
