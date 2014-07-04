{-# LANGUAGE ConstraintKinds #-}
{- |
Module      :  Lambda.PrimOp
Description :  Primitive operations.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-} 

module Lambda.PrimOp where

import Data.IORef
import Text.PrettyPrint (render)
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lambda.Name
import Lambda.Type
import Lambda.Object
import Lambda.Eval
import Lambda.PrettyPrint

debugPrint :: (PrettyPrint a, MonadEval m) => m a -> m a
debugPrint m = do
    val <- m
    liftIO . putStrLn . render . format $ val
    return val

-- | A list of build in type names.
buildInTypes :: Set Name
buildInTypes = Set.fromList ["()", "Bool", "Char", "Int", "Double"]

-- | Primitive constructors.
data PrimCons m = PrimCons Name ([TypeScheme], Type) (m (LObject m))

-- The only unit constructor.
primUnit :: MonadEval m => PrimCons m
primUnit = PrimCons "()"
    ([], tUnit)
    (newObj $ OCons "()" [])

-- | True.
primTrue :: MonadEval m => PrimCons m
primTrue = PrimCons "True"
    ([], tBool)
    (newObj $ OCons "True" [])

-- | False.
primFalse :: MonadEval m => PrimCons m
primFalse = PrimCons "False"
    ([], tBool)
    (newObj $ OCons "False" [])

primCons :: MonadEval m => [PrimCons m]
primCons =
    [ primUnit
    , primTrue
    , primFalse
    ]

-- | Primitive operation.
data PrimOp m = PrimOp Name TypeScheme (m (LObject m))

-- | Undefined value, that is bottom.
primUndefined :: MonadEval m => PrimOp m
primUndefined = PrimOp "undefined"
    (TypeScheme (Set.singleton "a") (TVar "a"))
    (newBot "undefined")

-- | Error, that is bottom with a message.
primError :: MonadEval m => PrimOp m
primError = PrimOp "error"
    (TypeScheme (Set.singleton "a") (TFun tString (TVar "a")))
    (newObj . OFun $ \x -> do
        msg <- evalStr x
        newBot msg)

primSeq :: MonadEval m => PrimOp m
primSeq = PrimOp "seq"
    (TypeScheme (Set.fromList ["a", "b"]) (TFun (TVar "a") (TFun (TVar "b") (TVar "b"))))
    (newObj . OFun $ \x -> do
        newObj . OFun $ \y -> do
            newObj $ OSeq x y)

-- | Lifts a haskell binary function operating on Int's into a PrimOp.
liftIntOp :: MonadEval m => Name -> (Int -> Int -> Int) -> PrimOp m
liftIntOp name op = PrimOp name
    (TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    (newObj . OFun $ \x -> do
        newObj . OFun $ \y -> do
            OInt x' <- eval x
            OInt y' <- eval y
            newObj . OInt $ (x' `op` y'))

-- | Lifts a haskell binary predicate operating on Int's into a PrimOp.
liftIntPred :: MonadEval m => Name -> (Int -> Int -> Bool) -> PrimOp m
liftIntPred name op = PrimOp name
    (TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))
    (newObj . OFun $ \x -> do
        newObj . OFun $ \y -> do
            OInt x' <- eval x
            OInt y' <- eval y
            if x' `op` y'
               then newObj $ OCons "True" []
               else newObj $ OCons "False" [])

-- Int operators.
primAdd :: MonadEval m => PrimOp m
primAdd = liftIntOp "+" (+)

primSub :: MonadEval m => PrimOp m
primSub = liftIntOp "-" (-)

primMul :: MonadEval m => PrimOp m
primMul = liftIntOp "*" (*)

primDiv :: MonadEval m => PrimOp m
primDiv = liftIntOp "/" quot

primLt :: MonadEval m => PrimOp m
primLt = liftIntPred "<" (<)

primLtEq :: MonadEval m => PrimOp m
primLtEq = liftIntPred "<=" (<=)

primGt :: MonadEval m => PrimOp m
primGt = liftIntPred ">" (>)

primGtEq :: MonadEval m => PrimOp m
primGtEq = liftIntPred ">=" (>=)

-- | Equality operator.
primEq :: MonadEval m => PrimOp m
primEq = PrimOp "=="
    (TypeScheme (Set.singleton "a") (TFun (TVar "a") (TFun (TVar "a") tBool)))
    (newObj . OFun $ \a ->
        newObj . OFun $ \b -> do
            b <- objEq a b
            if b
               then newObj $ OCons "True" []
               else newObj $ OCons "False" [])

-- List functions.
primListCons :: MonadEval m => PrimOp m
primListCons = PrimOp ":"
    (TypeScheme (Set.singleton "a") (TFun (TVar "a") (TFun (TList (TVar "a")) (TList (TVar "a")))))
    (newObj . OFun $ \el ->
        newObj . OFun $ \list -> do
            OList list' <- eval list
            newObj . OList $ el:list')

primHead :: MonadEval m => PrimOp m
primHead = PrimOp "head"
    (TypeScheme (Set.singleton "a") (TFun (TList (TVar "a")) (TVar "a")))
    (newObj . OFun $ \list -> do
        OList list' <- eval list
        case list' of
             []  -> newBot "head: empty list"
             x:_ -> return x)

primTail :: MonadEval m => PrimOp m
primTail = PrimOp "tail"
    (TypeScheme (Set.singleton "a") (TFun (TList (TVar "a")) (TList (TVar "a"))))
    (newObj . OFun $ \list -> do
        OList list' <- eval list
        case list' of
             []   -> newBot "tail: empty list"
             _:xs -> newObj . OList $ xs)

primNull :: MonadEval m => PrimOp m
primNull = PrimOp "null"
    (TypeScheme (Set.singleton "a") (TFun (TList (TVar "a")) tBool))
    (newObj . OFun $ \list -> do
        OList list' <- eval list
        if null list'
           then newObj $ OCons "True" []
           else newObj $ OCons "False" [])

-- | IO actions.
primPrintInt :: MonadEval m => PrimOp m
primPrintInt = PrimOp "printInt"
    (TypeScheme Set.empty (TFun tInt tUnit))
    (newObj . OFun $ \x -> do
        OInt x' <- eval x
        liftIO $ putStr (show x')
        newObj $ OCons "()" [])

primPrintStr :: MonadEval m => PrimOp m
primPrintStr = PrimOp "printStr"
    (TypeScheme Set.empty (TFun tString tUnit))
    (newObj . OFun $ \obj -> do
        str <- evalStr obj
        liftIO $ putStr str
        newObj $ OCons "()" [])

-- | A list of all PrimOps.
primOps :: MonadEval m => [PrimOp m]
primOps = 
    [ primUndefined
    , primError
    , primSeq
    , primAdd
    , primSub
    , primMul
    , primDiv
    , primLt
    , primLtEq
    , primGt
    , primGtEq
    , primEq
    , primListCons
    , primHead
    , primTail
    , primNull
    , primPrintInt
    , primPrintStr
    ]
