{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Interpreter (eval, parseEval, topEval) where

import Control.Monad
import Control.Monad.Except (Except, throwError)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Error.Class (MonadError)

import Binops (applyBinop)
import CoreTypes
import Parser (topParse)

-- Parse and evaluate a full program, returning the final value.
topEval :: String -> Except String Value
topEval = topParse >=> (`evalStateT` emptyStore) . eval emptyEnvironment

-- Parse and evaluate a full program,
-- returning the final value and the store.
parseEval :: String -> Except String (Value, Store)
parseEval = topParse >=> (`runStateT` emptyStore) . eval emptyEnvironment

-- Evaluate an expression in the given environment.
eval :: (MonadError String m, MonadState Store m) =>
    Environment -> ExprC -> m Value
eval _ (ValueC v) = return v
eval env (BinopC op l r) = do
    lval <- eval env l
    rval <- eval env r
    applyBinop op lval rval
eval env (IfC guard true false) = eval env guard >>= \case
    BoolV b -> eval env $ if b then true else false
    other   -> typeError "boolean value" "conditional expression" other
eval env (IdC id) = case envLookup id env of
    Nothing -> throwError $ "unbound identifier: " ++ show id
    Just a  -> storeLookup a >>= \case
        Nothing -> throwError $ concat
            [ "dangling pointer at store location ", show a, " "
            , "(referenced by identifier ", show id, ")"
            ]
        Just v  -> return v
eval env (LambdaC params body) = return $ ClosureV params body env
eval env (AppC fun args) = eval env fun >>= \case
    ClosureV params body clenv -> do
        unless (length params == length args) $ throwError $ concat
            [ "wrong arity to closure: "
            , "expected ", show $ length params, ", "
            , "but got ", show $ length args
            ]
        addrs <- mapM (eval env >=> allocate) args
        let env' = foldl (flip $ uncurry envBind) clenv (zip params addrs)
        eval env' body
eval env (NewArrayC lenExpr elExpr) = do
    el <- eval env elExpr
    eval env lenExpr >>= \case
        NumV len    -> flip ArrayV len <$> allocateMany len el
        other       -> typeError "numeric value" "new-array" other
eval env (SeqC exprs) = last <$> mapM (eval env) exprs
eval env (SetC id val) = case envLookup id env of
    Nothing -> throwError $ "unbound identifier: " ++ show id
    Just a  -> eval env val >>= storeSet a

typeError :: MonadError String m => Show a => String -> String -> a -> m b
typeError expected place actual = throwError $ concat
    [ "type error: "
    , "expected ", expected, " in ", place, ", "
    , "but found: ", show actual
    ]
