{- |
Module      :  Lambda.Errors
Description :  Error types.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}
 
module Lambda.Errors where

import Lambda.Name
import Lambda.Type
import Lambda.Syntax
import Lambda.SourceLoc

-- | Inference error.
data IError = UnificationError SourceRange Type Type
            | OccursCheckError SourceRange Name Type
            | UnboundVariable SourceRange Name
            | UnboundConstructor SourceRange Name
            | UnboundType SourceRange Name
            | InvalidPattern SourceRange Pattern
            | Redefinition SourceRange Name
            | InvalidConstructor SourceRange Name
    deriving (Eq, Show)
