{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Lambda.Inference
Description :  Type inference.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Lambda.Inference where

import Data.Text (pack)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Lambda.Name
import Lambda.Type

-- Errors.
data IError = IUnifyError

-- Inference environment.
data IEnv = IEnv

-- Inference state.
data IState = IState
    { _tVarCounter :: Int 
    }
makeLenses ''IState

-- Inference monad class.
class (MonadError IError m, MonadReader IEnv m, MonadState IState m) => MonadInfer m where
    -- Creates a new unique type variable.
    newTVar :: m Type
    newTVar = tVarCounter <<%= (+1) >>= return . TVar . fromString . ('a':) . show

-- Inference monad implementation.
type Infer a = ExceptT IError (ReaderT IEnv (State IState)) a
