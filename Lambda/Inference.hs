{-# LANGUAGE TemplateHaskell, ConstraintKinds #-}
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

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.String
import Data.Monoid
import Data.Text (pack)
import Data.Map (Map)
import Data.Set (Set)
import Data.Functor.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lambda.Name
import Lambda.Type
import Lambda.Syntax

-- Errors.
data IError = TUnificationError Type Type
            | TOccursCheckError Name Type
            | TUnboundVariable Name
    deriving (Eq, Ord, Show)

-- Inference environment.
data IEnv = IEnv
    { _bindings :: Map Name QuantType
    }
makeLenses ''IEnv

instance HasFreeTVars IEnv where
    freeTVars = bindings . freeTVars

insertBinding :: Name -> QuantType -> IEnv -> IEnv
insertBinding name qtype env = env & bindings . at name .~ Just qtype

removeBinding :: Name -> IEnv -> IEnv
removeBinding name env = env & bindings . at name .~ Nothing

generalize :: MonadReader IEnv m => Type -> m QuantType
generalize t = do
    binds <- view bindings
    return $ QuantType (setOfFreeTVars t `Set.difference` setOfFreeTVars binds) t

-- Inference state.
data IState = IState
    { _tVarCounter :: Int 
    }
makeLenses ''IState

-- Creates a new unique type variable.
newTVar :: MonadState IState m => m Type
newTVar = tVarCounter <<%= (+1) >>= return . TVar . fromString . ('a':) . show

-- Inference monad class.
type MonadInfer m = (MonadError IError m, MonadReader IEnv m, MonadState IState m)

instantiate :: MonadInfer m => QuantType -> m Type
instantiate (QuantType q t) = do
    list <- mapM (\name -> newTVar >>= return . ((,) name)) (Set.toAscList q)
    let subst = TSubst . Map.fromDistinctAscList $ list
    return $ apply subst t

tVarBinding :: MonadInfer m => Name -> Type -> m TSubst
tVarBinding n t | TVar n == t                     = return mempty
                | Set.member n (setOfFreeTVars t) = throwError $ TOccursCheckError n t
                | otherwise                       = return . TSubst $ Map.singleton n t

mgu :: MonadInfer m => Type -> Type -> m TSubst
mgu (TFun l r) (TFun l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return $ s1 <> s2
mgu (TList t1) (TList t2) = mgu t1 t2
mgu (TVar n) t = tVarBinding n t
mgu t (TVar n) = tVarBinding n t
mgu (TLit n1) (TLit n2) = 
    if n1 == n2 
       then return mempty
       else throwError $ TUnificationError (TLit n1) (TLit n2)
mgu t1 t2 = throwError $ TUnificationError t1 t2

inferLit :: Literal -> Type
inferLit (LitChar _) = tChar
inferLit (LitString _) = TList tChar
inferLit (LitInteger _) = tInt
inferLit (LitDouble _) = tDouble

-- Inference algebra.
inferA :: MonadInfer m => UExprF (m (TSubst, TExpr)) -> m (TSubst, TExpr)
inferA (EVar n) = do
    mqt <- view $ bindings . at n
    case mqt of
         Just qt -> do
             t <- instantiate qt
             return (mempty, tEVar t n)
         Nothing -> throwError $ TUnboundVariable n
inferA (ELit lit) = return (mempty, tELit (inferLit lit) lit)
inferA (EAbs n e) = do
    tVar <- newTVar
    (s, te) <- local (insertBinding n (QuantType Set.empty tVar)) e
    return (s, tEAbs (TFun (apply s tVar) (typeOf te)) n te)
inferA (EApp e1 e2) = do
    tVar <- newTVar
    (s1, te1) <- e1
    (s2, te2) <- local (apply s1) e2
    s3 <- mgu (apply s2 (typeOf te1)) (TFun (typeOf te2) tVar)
    return (s3 <> s2 <> s1, tEApp (apply s3 tVar) te1 te2)
inferA (ELet n e1 e2) = do
    (s1, te1) <- e1
    qtype <- local (apply s1) generalize (typeOf te1)
    (s2, te2) <- local (apply s1 . insertBinding n qtype) e2
    return (s1 <> s2, tELet (typeOf te2) n te1 te2)

infer :: MonadInfer m => UExpr -> m TExpr
infer expr = do
    (_, t) <- cata inferA expr
    return t

-- Inference monad implementation.
type Infer a = ExceptT IError (ReaderT IEnv (State IState)) a

runInfer :: Infer a -> Either IError a
runInfer i = fst $ runState (runReaderT (runExceptT i) initEnv) initState
    where
        initEnv = IEnv { _bindings = Map.singleton "+" (QuantType Set.empty (TFun tInt (TFun tInt tInt))) }
        initState = IState { _tVarCounter = 0 }
