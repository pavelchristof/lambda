{- |
Module      :  Lambda.Name
Description :  Binding name.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Lambda.Name where

import Data.Text (Text)

-- Use a simple text for now.
newtype Name = Name Text
    deriving (Eq, Show, Read)
