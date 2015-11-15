{-# LANGUAGE LambdaCase #-}

module Interpreter (eval, parseEval, topEval) where

import Control.Monad
import Control.Monad.Except (Except, throwError)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Control.Monad.Trans (lift)

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
eval :: Environment -> ExprC -> StateT Store (Except String) Value
eval _ (ValueC v) = return v
eval env (BinopC op l r) = do
    lval <- eval env l
    rval <- eval env r
    lift $ applyBinop op lval rval
{-
eval env (IdC id) = case envLookup id env of
    Just v  -> Right v
    Nothing -> Left $ "unbound identifier: " ++ show id
eval env (LambdaC params body) = Right $ ClosureV params body env
eval env (AppC fun args) = eval env fun >>= \case
    ClosureV params body clenv -> do
        unless (length params == length args) $ Left $ concat
            [ "wrong arity to closure: "
            , "expected ", show $ length params, ", "
            , "but got ", show $ length args
            ]
        argVals <- mapM (eval env) args
        let newEnv = foldl (flip $ uncurry envBind) clenv (zip params argVals)
        eval newEnv body
    other -> typeError "closure value" "function application" other
eval env (IfC guard true false) = do
    guardVal <- eval env guard
    case guardVal of
        BoolV b -> eval env $ if b then true else false
        other   -> typeError "boolean value" "conditional expression" other
-}
eval env (NewArrayC lenExpr elExpr) = do
    el <- eval env elExpr
    eval env lenExpr >>= \case
        NumV len    -> flip ArrayV len <$> allocateMany len el
        other       -> lift $ typeError "numeric value" "new-array" other
eval _ _ = lift $ throwError "not yet implemented"

typeError :: Show a => String -> String -> a -> Except String b
typeError expected place actual = throwError $ concat
    [ "type error: "
    , "expected ", expected, " in ", place, ", "
    , "but found: ", show actual
    ]
