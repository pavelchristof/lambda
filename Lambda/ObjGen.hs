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
import Data.IORef
import Data.Map (Map)
import Data.Text (unpack)
import Data.Functor.Foldable
import qualified Data.Map as Map

import Lambda.Syntax
import Lambda.Name
import Lambda.Object

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
         LitUnit -> return OUnit
         LitBool val -> return $ OBool val
         LitChar val -> return $ OChar val
         LitString text -> do
             list <- liftIO $ mapM (newIORef . Right . OChar) (unpack text)
             return $ OList list
         LitInteger val -> return $ OInt (fromInteger val)
         LitDouble val -> return $ ODouble val
         LitEmptyList -> return $ OList []

-- | F-algebra for expressions.
objGenExprA :: MonadIO e => TPExprF (ObjGen e (LObject e)) -> ObjGen e (LObject e)
objGenExprA (EVar _ name) = do 
    Just obj <- view $ bindings . at name
    return obj
objGenExprA (ELit _ lit) = do
    obj <- objGenLiteral lit
    liftIO . newIORef . Right $ obj
objGenExprA (EAbs _ name body) = do
    b <- view bindings
    liftIO $ newIORef . Right . OFun $ \obj ->
        liftIO $ runObjGen body (Map.insert name obj b)
objGenExprA (EApp _ f x) = do
    fObj <- f
    xObj <- x
    liftIO $ newIORef . Right $ OThunk fObj xObj
objGenExprA (ELet _ name f g) = do
    fObj <- f
    local (bindings . at name .~ Just fObj) g
objGenExprA (EFix _ name body) = do
    b <- view bindings
    liftIO $ mfix $ \obj -> runObjGen body (Map.insert name obj b)

-- | Generates an object from an expression.
objGenExpr :: MonadIO e => TPExpr -> ObjGen e (LObject e)
objGenExpr = cata objGenExprA

-- | F-algebra for statement lists.
objGenStmtsA :: MonadIO e => Stmt TPExpr -> ObjGen e (LObject e) -> ObjGen e (LObject e)
objGenStmtsA (SLet _ name f) g = do
    fObj <- objGenExpr f
    local (bindings . at name .~ Just fObj) g
objGenStmtsA (SEval _ f) g = do
    fObj <- objGenExpr f
    gObj <- g
    liftIO . newIORef . Right $ OSeq fObj gObj

-- | Generates an object from a list of statements.
objGenStmts :: MonadIO e => [Stmt TPExpr] -> ObjGen e (LObject e)
objGenStmts = foldr objGenStmtsA (liftIO . newIORef . Right $ OUnit)
