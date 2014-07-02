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

import Data.IORef
import Control.Monad.IO.Class

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
type LObject m = IORef (Either String (Object m))

-- | Creates a new bottom.
newBot :: MonadIO m => String -> m (LObject e)
newBot s = liftIO $ newIORef $ Left $ s

-- | Creates a new object.
newObj :: MonadIO m => Object e -> m (LObject e)
newObj o = liftIO $ newIORef $ Right $ o
