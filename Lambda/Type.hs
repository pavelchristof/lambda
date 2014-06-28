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

import Data.Text (Text)

-- Use a simple text for now.
newtype Type = Type Text
    deriving (Eq, Show, Read)
