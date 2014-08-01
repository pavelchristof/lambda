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

import Data.List (find)
import Data.IORef
import Control.Monad.Except
import Control.Monad.IO.Class

import Lambda.Object
import Lambda.Syntax
import Lambda.SourceLoc
import Lambda.Name

-- | The monad class for program evaluation.
type MonadEval m = (MonadError String m, MonadIO m)

-- | Evaluation monad.
type Eval = ExceptT String IO

-- | Runs the evaluation monad.
runEval :: Eval a -> IO (Either String a)
runEval = runExceptT

-- | Evaluates an object to weak head normal form.
eval :: MonadEval m => LObject m -> m (Object m)
eval ref = do
    obj <- liftIO $ readIORef ref
    res <- case obj of
         Left msg -> throwError msg
         Right (OThunk f x) -> do
             OFun f' <- eval f
             r <- f' x
             eval r
         Right (OSeq f g) -> do
             eval f
             eval g
         Right (OCase e p) -> do
             OCons n args <- eval e
             let matching ((Decons consName _), _) = n == consName
                 matching ((Wildcard _), _) = True
             case find matching p of
                  Just (pattern, resExpr) -> do
                      let names = case pattern of
                                       Decons _ names -> map unLoc names
                                       Wildcard name -> [name]
                      r <- callCase names args resExpr
                      eval r 
                  Nothing -> throwError "Pattern matching failure."
         Right obj -> return obj
    writeIORef ref (Right res)
    return res

callCase :: MonadEval m => [Maybe Name] -> [LObject m] -> LObject m -> m (LObject m)
callCase [] _ o = return o
callCase (n:ns) (a:as) f =
    case n of
         Just _ -> do
             OFun f' <- eval f
             r <- f' a
             callCase ns as r
         Nothing -> callCase ns as f

-- | Compares two objects. This is a partial function. Only primitive types are comparable.
-- A primitive type is either:
--     - A Char, Int, Double,
--     - A list over a primitive type,
--     - A constructor applied to primitive types.
objEq :: MonadEval m => LObject m -> LObject m -> m Bool
objEq a b = do
    a' <- eval a
    b' <- eval b
    case (a', b') of
         (OChar a, OChar b)         -> return $ a == b
         (OInt a, OInt b)           -> return $ a == b
         (ODouble a, ODouble b)     -> return $ a == b
         (OList a, OList b)         -> objListEq a b
         (OCons n1 l1, OCons n2 l2) -> if n1 /= n2 then return False else objListEq l1 l2
         _                          -> throwError "Trying to compare nonprimitive types."

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
