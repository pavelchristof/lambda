{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}
{- |
Module      :  Lambda.ObjGen
Description :  Object generation.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-} 

module Lambda.ObjGen where

import Control.Lens
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Scientific
import Data.IORef
import Data.Map (Map)
import Data.Text.Lazy (unpack)
import Data.Functor.Foldable
import qualified Data.Map as Map

import Lambda.Syntax
import Lambda.Name
import Lambda.Object
import Lambda.SourceLoc

-- | State.
data OGEnv e = OGEnv
    { _bindings :: Map Name (LObject e)
    }
makeLenses ''OGEnv

-- | Object generation monad.
type ObjGen e = ReaderT (OGEnv e) IO

-- | Runs object generation.
runObjGen :: Monad e => ObjGen e a -> Map Name (LObject e) -> IO a
runObjGen m b = runReaderT m (OGEnv b)

-- | Generates an object for a literal.
objGenLiteral :: Literal -> ObjGen e (Object e)
objGenLiteral lit =
    case lit of
         LitChar val -> return $ OChar val
         LitString text -> do
             list <- liftIO $ mapM (newIORef . Right . OChar) (unpack text)
             return $ OList list
         LitInteger val -> return $ OInt (fromInteger val)
         LitReal val -> return $ ODouble (toRealFloat val)
         LitEmptyList -> return $ OList []

-- | F-algebra for expressions.
objGenExprA :: MonadIO e => TLExprF (ObjGen e (LObject e)) -> ObjGen e (LObject e)
objGenExprA (EVar _ name) = do
    Just obj <- view $ bindings . at name
    return obj
objGenExprA (ELit _ lit) = do
    obj <- objGenLiteral lit
    newObj obj
objGenExprA (EAbs _ name body) = do
    b <- view bindings
    newObj . OFun $ \obj ->
        liftIO $ runObjGen body (Map.insert name obj b)
objGenExprA (EApp _ f x) = do
    fObj <- f
    xObj <- x
    newObj $ OThunk fObj xObj
objGenExprA (ELet _ name f g) = do
    b <- view bindings
    fObj <- liftIO $ mfix (\obj -> runObjGen f (Map.insert name obj b))
    local (bindings . at name .~ Just fObj) g
objGenExprA (ECase _ e p) = do
    eObj <- e
    caseObjs <- mapM objGenCase p
    newObj $ OCase eObj caseObjs

-- | Generates a case branch.
objGenCase :: MonadIO e => (Located Pattern, ObjGen e (LObject e)) -> ObjGen e (Pattern, LObject e)
objGenCase (L _ p, e) = do
    let names = case p of
                     Decons _ names -> map unLoc names
                     Wildcard name -> [name]
    eObj <- objGenCaseE names e
    return (p, eObj)

-- | Generates a function object that takes some arguments and evaluates expr with them bound to names.
objGenCaseE :: MonadIO e => [Maybe Name] -> ObjGen e (LObject e) -> ObjGen e (LObject e)
objGenCaseE [] expr = expr
objGenCaseE ((Just name):names) expr = do
    b <- view bindings
    newObj $ OFun (\obj ->
        liftIO $ runObjGen (objGenCaseE names expr) (Map.insert name obj b))
objGenCaseE (Nothing:names) expr = objGenCaseE names expr

-- | Generates an object from an expression.
objGenExpr :: MonadIO e => TLExpr -> ObjGen e (LObject e)
objGenExpr = cata objGenExprA

-- | F-algebra for statement lists.
objGenDeclsA :: MonadIO e => Decl TLExpr -> ObjGen e (Map Name (LObject e)) -> ObjGen e (Map Name (LObject e))
objGenDeclsA (DAssign name f) g = do
    b <- view bindings
    fObj <- liftIO $ mfix (\obj -> runObjGen (objGenExpr f) (Map.insert name obj b))
    local (bindings . at name .~ Just fObj) g
objGenDeclsA (DData name consDefs) g = do
    let genConsObj (ConsDef consName args) = do
            obj <- genConsFun consName (length args) []
            return (consName, obj)
    consObjs <- mapM genConsObj (map unLoc consDefs)
    let addBinding env (consName, consObj) =
            env & bindings . at consName .~ Just consObj
    env <- ask
    let env' = foldl addBinding env consObjs
    local (const env') g

-- Generates a constructor.
genConsFun :: MonadIO e => Name -> Int -> [LObject e] -> ObjGen e (LObject e)
genConsFun name 0 args = newObj $ OCons name (reverse args)
genConsFun name n args = do
    b <- view bindings
    newObj $ OFun (\arg -> liftIO $ runObjGen (genConsFun name (n - 1) (arg:args)) b)

-- | Generates object bindings from a list of statements.
objGenDecls :: MonadIO e => [Decl TLExpr] -> ObjGen e (Map Name (LObject e))
objGenDecls = foldr objGenDeclsA (view bindings)
