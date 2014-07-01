{-# LANGUAGE ConstraintKinds #-}
{- |
Module      :  Lambda.Eval
Description :  Object evaluation.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-} 

module Lambda.Eval where

import Control.Monad.Except
import Control.Monad.IO.Class

import Lambda.Object

-- | The monad class for program evaluation.
type MonadEval m = (MonadError String m, MonadIO m)

-- | Evaluation monad.
type Eval = ExceptT String IO

-- | Runs the evaluation monad.
runEval :: Eval a -> IO (Either String a)
runEval = runExceptT

-- | Evaluates an object to weak head normal form.
eval :: MonadEval m => LObject m -> m (Object m)
eval (Left msg) = throwError msg
eval (Right (OThunk f x)) = do
    OFun f' <- eval f
    r <- f' x
    eval r
eval (Right (OSeq f g)) = do
    f' <- eval f
    eval g
eval (Right obj) = return obj

-- | Compares two objects. This is a partial function. Only primitive types are comparable.
-- A primitive type is either:
--     - A Unit, Bool, Char, Int, Double,
--     - A list over a primitive type.
objEq :: MonadEval m => LObject m -> LObject m -> m Bool
objEq a b = do
    a' <- eval a
    b' <- eval b
    case (a', b') of
         (OUnit, OUnit)         -> return True
         (OBool a, OBool b)     -> return $ a == b
         (OChar a, OChar b)     -> return $ a == b
         (OInt a, OInt b)       -> return $ a == b
         (ODouble a, ODouble b) -> return $ a == b
         (OList a, OList b )    -> objListEq a b
         _                      -> throwError "Trying to compare nonprimitive types."

-- | Compares two lists of objects. See objEq.
objListEq :: MonadEval m => [LObject m] -> [LObject m] -> m Bool
objListEq [] [] = return True
objListEq [] _  = return False
objListEq _ []  = return False
objListEq (x:xs) (y:ys) = do
    eqHead <- objEq x y
    if not eqHead
       then return False
       else do
           eqTail <- objListEq xs ys
           return eqTail

-- | Fully evaluates a char.
evalChar :: MonadEval m => LObject m -> m Char
evalChar obj = do
    OChar char <- eval obj
    return char

-- | Fully evalates a string.
evalStr :: MonadEval m => LObject m -> m String
evalStr obj = do
    OList chars <- eval obj
    str <- mapM evalChar chars
    return str
