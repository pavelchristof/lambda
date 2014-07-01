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

import Text.PrettyPrint (render)
import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lambda.Name
import Lambda.Type
import Lambda.Object
import Lambda.Eval
import Lambda.PrettyPrint

data PrimOp m = PrimOp Name TypeScheme (LObject m)

debugPrint :: (PrettyPrint a, MonadEval m) => m a -> m a
debugPrint m = do
    val <- m
    liftIO . putStrLn . render . format $ val
    return val

-- | Undefined value, that is bottom.
primUndefined :: MonadEval m => PrimOp m
primUndefined = PrimOp "undefined"
    (TypeScheme (Set.singleton "a") (TVar "a"))
    (Left "undefined")

-- | Error, that is bottom with a message.
primError :: MonadEval m => PrimOp m
primError = PrimOp "error"
    (TypeScheme (Set.singleton "a") (TFun tString (TVar "a")))
    (Right . OFun $ \x -> do
        msg <- evalStr x
        return . Left $ msg)

primSeq :: MonadEval m => PrimOp m
primSeq = PrimOp "seq"
    (TypeScheme (Set.fromList ["a", "b"]) (TFun (TVar "a") (TFun (TVar "b") (TVar "b"))))
    (Right . OFun $ \x -> do
        return . Right . OFun $ \y -> do
            return . Right $ OSeq x y)

-- | Lifts a haskell binary function operating on Int's into a PrimOp.
liftIntOp :: MonadEval m => Name -> (Int -> Int -> Int) -> PrimOp m
liftIntOp name op = PrimOp name
    (TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    (Right . OFun $ \x -> do
        return . Right . OFun $ \y -> do
            OInt x' <- eval x
            OInt y' <- eval y
            return . Right . OInt $ (x' `op` y'))

-- | Lifts a haskell binary predicate operating on Int's into a PrimOp.
liftIntPred :: MonadEval m => Name -> (Int -> Int -> Bool) -> PrimOp m
liftIntPred name op = PrimOp name
    (TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))
    (Right . OFun $ \x -> do
        return . Right . OFun $ \y -> do
            OInt x' <- eval x
            OInt y' <- eval y
            return . Right . OBool $ (x' `op` y'))

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
    (Right . OFun $ \a ->
        return . Right . OFun $ \b ->
            objEq a b >>= return . Right . OBool)

-- | If instruction.
primIf :: MonadEval m => PrimOp m
primIf = PrimOp "if"
    (TypeScheme (Set.singleton "a") (TFun tBool (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))))
    (Right . OFun $ \cond -> do
        return . Right . OFun $ \th ->
            return . Right . OFun $ \el -> do
                OBool val <- eval cond
                if val
                   then return th
                   else return el)

-- List functions.
primPrepend :: MonadEval m => PrimOp m
primPrepend = PrimOp ":"
    (TypeScheme (Set.singleton "a") (TFun (TVar "a") (TFun (TList (TVar "a")) (TList (TVar "a")))))
    (Right . OFun $ \el ->
        return . Right . OFun $ \list -> do
            OList list' <- eval list
            return . Right . OList $ el:list')

primHead :: MonadEval m => PrimOp m
primHead = PrimOp "head"
    (TypeScheme (Set.singleton "a") (TFun (TList (TVar "a")) (TVar "a")))
    (Right . OFun $ \list -> do
        OList list' <- eval list
        case list' of
             []  -> return . Left $ "head: empty list"
             x:_ -> return x)

primTail :: MonadEval m => PrimOp m
primTail = PrimOp "tail"
    (TypeScheme (Set.singleton "a") (TFun (TList (TVar "a")) (TList (TVar "a"))))
    (Right . OFun $ \list -> do
        OList list' <- eval list
        case list' of
             []   -> return . Left $ "tail: empty list"
             _:xs -> return . Right . OList $ xs)

primNull :: MonadEval m => PrimOp m
primNull = PrimOp "null"
    (TypeScheme (Set.singleton "a") (TFun (TList (TVar "a")) tBool))
    (Right . OFun $ \list -> do
        OList list' <- eval list
        return . Right . OBool $ null list')

-- | IO actions.
primPrintInt :: MonadEval m => PrimOp m
primPrintInt = PrimOp "printInt"
    (TypeScheme Set.empty (TFun tInt tUnit))
    (Right . OFun $ \x -> do
        OInt x' <- eval x
        liftIO $ print x'
        return $ Right $ OUnit)

primPrintStr :: MonadEval m => PrimOp m
primPrintStr = PrimOp "printStr"
    (TypeScheme Set.empty (TFun tString tUnit))
    (Right . OFun $ \obj -> do
        str <- evalStr obj
        liftIO $ putStr str
        return $ Right $ OUnit)

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
    , primIf
    , primPrepend
    , primHead
    , primTail
    , primNull
    , primPrintInt
    , primPrintStr
    ]
