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
    return $ s1 <> s2
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
inferLit :: Literal -> Type
inferLit LitUnit = tUnit
inferLit (LitBool _) = tBool
inferLit (LitChar _) = tChar
inferLit (LitString _) = TList tChar
inferLit (LitInteger _) = tInt
inferLit (LitDouble _) = tDouble

inferExprA :: MonadInfer m => PExprF (m (TSubst, TPExpr)) -> m (TSubst, TPExpr)
inferExprA (EVar pos n) = do
    mqt <- view $ bindings . at n
    case mqt of
         Just qt -> do
             t <- instantiate qt
             return (mempty, fEVar (pos, t) n)
         Nothing -> throwError $ UnboundVariable pos n
inferExprA (ELit pos lit) = return (mempty, fELit (pos, inferLit lit) lit)
inferExprA (EAbs pos n e) = do
    tVar <- newTVar
    (s, te) <- local (bindings . at n .~ Just (TypeScheme Set.empty tVar)) e
    return (s, fEAbs (pos, TFun (apply s tVar) (te^.typeOf)) n te)
inferExprA (EApp pos e1 e2) = do
    tVar <- newTVar
    (s1, te1) <- e1
    (s2, te2) <- local (apply s1) e2
    s3 <- local (sourcePos .~ Just pos) $ mgu (apply s2 (te1^.typeOf)) (TFun (te2^.typeOf) tVar)
    return (s3 <> s2 <> s1, fEApp (pos, apply s3 tVar) te1 te2)
inferExprA (ELet pos n e1 e2) = do
    (s1, te1) <- e1
    scheme <- local (apply s1) $ generalize (te1^.typeOf)
    (s2, te2) <- local (apply s1 . (bindings . at n .~ Just scheme)) e2
    return (s1 <> s2, fELet (pos, te2^.typeOf) n te1 te2)
inferExprA (EFix pos n x e) = do
    tVar <- newTVar
    (s1, te1) <- local (bindings . at n .~ Just (TypeScheme Set.empty tVar)) (inferExprA $ EAbs pos x e)
    s2 <- mgu (apply s1 tVar) (te1^.typeOf)
    return (s2 <> s1, fEFix (pos, (apply s2 (te1^.typeOf))) n x te1)

inferExpr :: MonadInfer m => PExpr -> m (TSubst, TPExpr)
inferExpr = cata inferExprA

inferStmtsA :: MonadInfer m => Stmt PExpr -> m (TSubst, [Stmt TPExpr]) -> m (TSubst, [Stmt TPExpr])
inferStmtsA (SLet pos name expr) cont = do
    (s1, tExpr) <- inferExpr expr
    scheme <- local (apply s1) $ generalize (tExpr^.typeOf)
    (s2, stmts) <- local (apply s1 . (bindings . at name .~ Just scheme)) cont
    return (s1 <> s2, (SLet pos name tExpr):stmts)
inferStmtsA (SEval pos expr) cont = do
    (s1, stmts) <- cont
    (s2, tExpr) <- local (apply s1) (inferExpr expr)
    return (s1 <> s2, (SEval pos tExpr):stmts)

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
