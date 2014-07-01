{-# LANGUAGE ConstraintKinds #-}
{- |
Module      :  Lambda.Object
Description :  Heap object.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}
 
module Lambda.Object where

-- | An unflited object.
data Object m = OFun ((LObject m) -> m (LObject m))
              | OThunk (LObject m) (LObject m)
              | OSeq (LObject m) (LObject m)
              | OUnit
              | OBool Bool
              | OChar Char
              | OInt Int
              | ODouble Double
              | OList [LObject m]

-- | A lifted object. Bottom is represented as Left.
type LObject m = Either String (Object m)
