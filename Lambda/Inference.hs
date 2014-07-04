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
import Data.String
import Data.Monoid
import Data.Text (pack)
import Data.Map (Map)
import Data.Set (Set)
import Data.Functor.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lambda.Name
import Lambda.PrimOp
import Lambda.Type
import Lambda.Eval (Eval)
import Lambda.Syntax
import Lambda.SourceLoc
import Lambda.Errors

-- Inference environment.
data IEnv = IEnv
    { _bindings :: Map Name TypeScheme
    , _sourcePos :: SourceRange
    , _constructors :: Map Name ([TypeScheme], Type)
    , _types :: Set Name
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
                    loc <- view sourcePos
                    throwError $ OccursCheckError loc n t
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
           loc <- view sourcePos
           throwError $ UnificationError loc (TLit n1) (TLit n2)
mgu t1 t2 = do
    loc <- view sourcePos
    throwError $ UnificationError loc t1 t2

-- Inference.
inferLit :: MonadInfer m => Literal -> m Type
inferLit (LitChar _) = return tChar
inferLit (LitString _) = return $ TList tChar
inferLit (LitInteger _) = return tInt
inferLit (LitReal _) = return tDouble
inferLit LitEmptyList = newTVar >>= return . TList

inferExprA :: MonadInfer m => LExprF (m (TSubst, TLExpr)) -> m (TSubst, TLExpr)
inferExprA (EVar loc x) = do
    binding <- view $ bindings . at x
    case binding of
         Just scheme -> do
             t0 <- instantiate scheme
             return (mempty, fEVar (loc, t0) x)
         Nothing -> throwError $ UnboundVariable loc x
inferExprA (ELit loc lit) = do
    t0 <- inferLit lit
    return (mempty, fELit (loc, t0) lit)
inferExprA (EApp loc f g) = do
    (s1, f') <- f
    (s2, g') <- local (apply s1) g
    b <- newTVar
    s3 <- local (sourcePos .~ loc) $ mgu (apply s2 (f'^.typeOf)) (TFun (g'^.typeOf) b)
    return (s3 <> s2 <> s1, fEApp (loc, apply s3 b) f' g')
inferExprA (EAbs loc x f) = do
    b <- newTVar
    (s1, f') <- local (bindings . at x .~ Just (TypeScheme Set.empty b)) f
    return (s1, fEAbs (loc, TFun (apply s1 b) (f'^.typeOf)) x f')
inferExprA (ELet loc x f g) = do
    b <- newTVar
    (s1, f') <- local (bindings . at x .~ Just (TypeScheme Set.empty b)) f
    s2 <- local (sourcePos .~ loc) $ mgu (apply s1 b) (f'^.typeOf)
    let s21 = s2 <> s1
    (s3, g') <- local (apply s21) $ do
        r <- generalize (apply s21 b)
        local (bindings . at x .~ Just r) g
    return (s3 <> s21, fELet (loc, g'^.typeOf) x f' g')
inferExprA (ECase loc e p) = do
    matchedType <- newTVar
    (s1, e') <- e
    s2 <- local (sourcePos .~ loc) $ mgu (apply s1 matchedType) (e'^.typeOf)
    resType <- newTVar
    let s = s2 <> s1
    (subs, matchedType', resType', p') <- foldM inferDecons (s2 <> s1, apply s matchedType, resType, []) p
    return (subs, fECase (loc, resType') e' (reverse p'))

inferDecons :: MonadInfer m 
    -- The fold accumulator, contains: the current substitusion, the matched expression type, the result type and 
    -- the typed pattern list.
    => (TSubst, Type, Type, [(Located Pattern, TLExpr)]) 
    -- The list element contains: the pattern and the result expression for that pattern.
    -> (Located Pattern, m (TSubst, TLExpr)) 
    -- The result is the next accumulator.
    -> m (TSubst, Type, Type, [(Located Pattern, TLExpr)])
inferDecons (subs, matchedType, resType, cases) (L loc pattern, resExpr) = do
    -- A function that inserts a binding into a binding map, for later.
    let addBinding m (L _ (Just var), varType) = Map.insert var varType m
        addBinding m (L _ Nothing, _) = m
    case pattern of
         Decons name vars -> do
             -- Check if the constructor exists.
             consMap <- view constructors
             case Map.lookup name consMap of
                  Just (varTypes, dataType) -> do
                      -- Check if the parameter count if ok.
                      when (length vars /= length varTypes) $ 
                          throwError $ InvalidPattern loc pattern
                      -- We can infer the matched type straight away, it must be dataType.
                      s1 <- local ((sourcePos .~ loc) . apply subs) $ mgu matchedType dataType
                      -- Parameter types are always fully inferred, just bind them to the names.
                      bindMap <- view bindings
                      let newBindMap = foldl addBinding bindMap (zip vars varTypes)
                      -- Now that we have these binding we can infer result expression type.
                      (s2, resExpr') <- local (apply (s1 <> subs) . (bindings .~ newBindMap)) resExpr
                      s3 <- local ((sourcePos .~ loc) . apply (s2 <> s1 <> subs)) $ mgu resType (resExpr'^.typeOf)
                      -- Everything done, lets return.
                      let s = s3 <> s2 <> s1 <> subs
                      return (s, dataType, apply s resType, (L loc pattern, resExpr'):cases)
                  Nothing -> throwError $ UnboundConstructor loc name
         Wildcard name -> do
             -- This is exactly like a non-recursive "let name = matchedExpr in resExpr", 
             -- but with some more constraints.
             (s1, resExpr') <- local (apply subs) $ do
                 case name of
                     Just name' -> do
                         matchedScheme <- generalize matchedType
                         local (bindings . at name' .~ Just matchedScheme) resExpr
                     Nothing -> resExpr
             -- But now we have to try to unify the result types.
             s2 <- local ((sourcePos .~ loc) . apply (s1 <> subs)) $ mgu resType (resExpr'^.typeOf)
             let s = s2 <> s1 <> subs
             return (s1 <> subs, apply s matchedType, apply s resType, (L loc pattern, resExpr'):cases)

inferExpr :: MonadInfer m => LExpr -> m (TSubst, TLExpr)
inferExpr = cata inferExprA

inferStmtsA :: MonadInfer m => Located (Decl LExpr) -> m (TSubst, [Located (Decl TLExpr)]) -> m (TSubst, [Located (Decl TLExpr)])
inferStmtsA (L loc (DAssign x f)) g = do
    binds <- view bindings
    when (Map.member x binds) $ 
        throwError $ Redefinition loc x
    b <- newTVar
    (s1, f') <- local (bindings . at x .~ Just (TypeScheme Set.empty b)) (inferExpr f)
    s2 <- local (sourcePos .~ loc) $ mgu (apply s1 b) (f'^.typeOf)
    let s21 = s2 <> s1
    (s3, g') <- local (apply s21) $ do
        r <- generalize (apply s21 b)
        local (bindings . at x .~ Just r) g
    return (s3 <> s21, (L loc (DAssign x f')):g')
inferStmtsA (L loc (DData name consDefs)) g = do
    ts <- view types
    when (Set.member name ts) $
        throwError $ Redefinition loc name
    local (types %~ Set.insert name) $ do
        mapM_ verifCons consDefs
        (s1, g') <- local (introCons name (map unLoc consDefs)) g
        return (s1, (L loc (DData name consDefs)):g')

introCons :: Name -> [ConsDef] -> IEnv -> IEnv
introCons dataName consList env = foldl f env consList
    where 
        f env def@(ConsDef consName params) =
            let env' = env & bindings . at consName .~ Just (TypeScheme Set.empty (consFunType (TVar dataName) params))
                env'' = env' & constructors . at consName .~ Just (map (TypeScheme Set.empty) params, TVar dataName)
            in env''

consFunType :: Type -> [Type] -> Type
consFunType dataType params = foldr TFun dataType params

verifCons :: MonadInfer m => Located ConsDef -> m ()
verifCons (L loc (ConsDef name params)) = do
    binds <- view bindings
    when (Map.member name binds) $ do
        throwError $ Redefinition loc name
    constru <- view constructors
    when (Map.member name constru) $ do
        throwError $ Redefinition loc name
    mapM_ (verifConsParam name loc) params

verifConsParam :: MonadInfer m => Name -> SourceRange -> Type -> m ()
verifConsParam n loc (TVar _) = throwError $ InvalidConstructor loc n
verifConsParam n loc (TLit t) = do
    ts <- view types
    when (not $ Set.member t ts) $ do
        throwError $ UnboundType loc t
verifConsParam n loc (TFun f g) = do
    verifConsParam n loc f
    verifConsParam n loc g
verifConsParam n loc (TList t) = verifConsParam n loc t

inferStmts :: MonadInfer m => [Located (Decl LExpr)] -> m (TSubst, [Located (Decl TLExpr)])
inferStmts = foldr inferStmtsA (return (mempty, []))

-- Inference monad implementation.
type Infer a = ExceptT IError (ReaderT IEnv (State IState)) a

runInfer :: Infer a -> Either IError a
runInfer i = fst $ runState (runReaderT (runExceptT i) initEnv) initState
    where
        initEnv = IEnv
            { _bindings = initBindings
            , _sourcePos = mempty
            , _constructors = initConstructors
            , _types = buildInTypes
            }
        initState = IState
            { _tVarCounter = 0
            }
        initBindings = Map.fromList (map extractBinding (primOps :: [PrimOp Eval]))
                     `Map.union`
                       Map.fromList (map extractConsBinding (primCons :: [PrimCons Eval]))
        extractBinding (PrimOp n t _) = (n, t)
        extractConsBinding (PrimCons n (params, dataType) _) = 
            (n, TypeScheme Set.empty $ consFunType dataType (map (\(TypeScheme _ t) -> t) params))
        initConstructors = Map.fromList $ map extractCons (primCons :: [PrimCons Eval])
        extractCons (PrimCons n t _) = (n, t)
