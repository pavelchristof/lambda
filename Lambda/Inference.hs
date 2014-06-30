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
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Text.Parsec.Pos (SourcePos)
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
data IError = UnificationError SourcePos Type Type
            | OccursCheckError SourcePos Name Type
            | UnboundVariable SourcePos Name
            | Redefinition SourcePos Name
    deriving (Eq, Ord, Show)

-- Inference environment.
data IEnv = IEnv
    { _bindings :: Map Name TypeScheme
    , _sourcePos :: Maybe SourcePos
    }
makeLenses ''IEnv

instance HasFreeTVars IEnv where
    freeTVars = bindings . freeTVars

generalize :: MonadReader IEnv m => Type -> m TypeScheme
generalize t = do
    binds <- view bindings
    return $ TypeScheme (setOfFreeTVars t `Set.difference` setOfFreeTVars binds) t

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

instantiate :: MonadInfer m => TypeScheme -> m Type
instantiate (TypeScheme q t) = do
    list <- mapM (\name -> newTVar >>= return . ((,) name)) (Set.toAscList q)
    let subst = TSubst . Map.fromDistinctAscList $ list
    return $ apply subst t

-- Most general unifier.
tVarBinding :: MonadInfer m => Name -> Type -> m TSubst
tVarBinding n t | TVar n == t                     = return mempty
                | Set.member n (setOfFreeTVars t) = do
                    Just pos <- view sourcePos
                    throwError $ OccursCheckError pos n t
                | otherwise                       = return . TSubst $ Map.singleton n t

mgu :: MonadInfer m => Type -> Type -> m TSubst
mgu (TFun l r) (TFun l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return $ s2 <> s1
mgu (TList t1) (TList t2) = mgu t1 t2
mgu (TVar n) t = tVarBinding n t
mgu t (TVar n) = tVarBinding n t
mgu (TLit n1) (TLit n2) = 
    if n1 == n2 
       then return mempty
       else do
           Just pos <- view sourcePos
           throwError $ UnificationError pos (TLit n1) (TLit n2)
mgu t1 t2 = do
    Just pos <- view sourcePos
    throwError $ UnificationError pos t1 t2

-- Inference.
inferLit :: MonadInfer m => Literal -> m Type
inferLit LitUnit = return tUnit
inferLit (LitBool _) = return tBool
inferLit (LitChar _) = return tChar
inferLit (LitString _) = return $ TList tChar
inferLit (LitInteger _) = return tInt
inferLit (LitDouble _) = return tDouble
inferLit LitEmptyList = newTVar >>= return . TList

inferExprA :: MonadInfer m => PExprF (m (TSubst, TPExpr)) -> m (TSubst, TPExpr)
inferExprA (EVar pos x) = do
    binding <- view $ bindings . at x
    case binding of
         Just scheme -> do
             t0 <- instantiate scheme
             return (mempty, fEVar (pos, t0) x)
         Nothing -> throwError $ UnboundVariable pos x
inferExprA (ELit pos lit) = do
    t0 <- inferLit lit
    return (mempty, fELit (pos, t0) lit)
inferExprA (EApp pos f g) = do
    (s1, f') <- f
    (s2, g') <- local (apply s1) g
    b <- newTVar
    s3 <- local (sourcePos .~ Just pos) $ mgu (apply s2 (f'^.typeOf)) (TFun (g'^.typeOf) b)
    return (s3 <> s2 <> s1, fEApp (pos, apply s3 b) f' g')
inferExprA (EAbs pos x f) = do
    b <- newTVar
    (s1, f') <- local (bindings . at x .~ Just (TypeScheme Set.empty b)) f
    return (s1, fEAbs (pos, TFun (apply s1 b) (f'^.typeOf)) x f')
inferExprA (EFix pos x f) = do
    b <- newTVar
    (s1, f') <- local (bindings . at x .~ Just (TypeScheme Set.empty b)) f
    s2 <- local (sourcePos .~ Just pos) $ mgu (apply s1 b) (f'^.typeOf)
    let s21 = s2 <> s1
    return (s21, fEFix (pos, apply s21 b) x f')
inferExprA (ELet pos x f g) = do
    (s1, f') <- f
    (s2, g') <- local (apply s1) $ do
        r <- generalize (f'^.typeOf)
        local (bindings . at x .~ Just r) g
    return (s2 <> s1, fELet (pos, g'^.typeOf) x f' g')

inferExpr :: MonadInfer m => PExpr -> m (TSubst, TPExpr)
inferExpr = cata inferExprA

inferStmtsA :: MonadInfer m => Stmt PExpr -> m (TSubst, [Stmt TPExpr]) -> m (TSubst, [Stmt TPExpr])
inferStmtsA (SLet pos x f) g = do
    (s1, f') <- inferExpr f
    (s2, g') <- local (apply s1) $ do
        r <- generalize (f'^.typeOf)
        local (bindings . at x .~ Just r) g
    return (s2 <> s1, (SLet pos x f'):g')
inferStmtsA (SEval pos f) g = do
    (s1, f') <- inferExpr f
    (s2, g') <- local (apply s1) g
    return (s2 <> s1, (SEval pos f'):g')

inferStmts :: MonadInfer m => [Stmt PExpr] -> m (TSubst, [Stmt TPExpr])
inferStmts = foldr inferStmtsA (return (mempty, []))

-- Inference monad implementation.
type Infer a = ExceptT IError (ReaderT IEnv (State IState)) a

runInfer :: Infer a -> Map Name TypeScheme -> Either IError a
runInfer i b = fst $ runState (runReaderT (runExceptT i) initEnv) initState
    where
        initEnv = IEnv
            { _bindings = b
            , _sourcePos = Nothing
            }
        initState = IState
            { _tVarCounter = 0
            }
