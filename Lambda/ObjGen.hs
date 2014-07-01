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
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Except
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
type ObjGen e = Reader (OGEnv e)

-- | Runs object generation.
runObjGen :: ObjGen e a -> Map Name (LObject e) -> a
runObjGen m b = runReader m (OGEnv b)

-- | Generates an object for a literal.
objGenLiteral :: Literal -> Object m
objGenLiteral lit =
    case lit of
         LitUnit -> OUnit
         LitBool val -> OBool val
         LitChar val -> OChar val
         LitString text -> OList (map (Right . OChar) (unpack text))
         LitInteger val -> OInt (fromInteger val)
         LitDouble val -> ODouble val
         LitEmptyList -> OList []

-- | F-algebra for expressions.
objGenExprA :: Applicative e => TPExprF (ObjGen e (LObject e)) -> ObjGen e (LObject e)
objGenExprA (EVar _ name) = do 
    Just obj <- view $ bindings . at name
    return obj
objGenExprA (ELit _ lit) = return . Right $ objGenLiteral lit
objGenExprA (EAbs _ name body) = do
    b <- view bindings
    return . Right . OFun $ \obj ->
        pure $ runObjGen body (Map.insert name obj b)
objGenExprA (EApp _ f x) = do
    fObj <- f
    xObj <- x
    return . Right $ OThunk fObj xObj
objGenExprA (ELet _ name f g) = do
    fObj <- f
    local (bindings . at name .~ Just fObj) g
objGenExprA (EFix _ name body) = do
    b <- view bindings
    let obj = runObjGen body (Map.insert name obj b)
    return obj

-- | Generates an object from an expression.
objGenExpr :: Applicative e => TPExpr -> ObjGen e (LObject e)
objGenExpr = cata objGenExprA

-- | F-algebra for statement lists.
objGenStmtsA :: Applicative e => Stmt TPExpr -> ObjGen e (LObject e) -> ObjGen e (LObject e)
objGenStmtsA (SLet _ name f) g = do
    fObj <- objGenExpr f
    local (bindings . at name .~ Just fObj) g
objGenStmtsA (SEval _ f) g = do
    fObj <- objGenExpr f
    gObj <- g
    return . Right $ OSeq fObj gObj

-- | Generates an object from a list of statements.
objGenStmts :: Applicative e => [Stmt TPExpr] -> ObjGen e (LObject e)
objGenStmts = foldr objGenStmtsA (return . Right $ OUnit)
