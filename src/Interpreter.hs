{-# LANGUAGE LambdaCase #-}

module Interpreter (eval, topEval) where

import Control.Monad
import Control.Monad.State (evalState)

import Binops (applyBinop)
import CoreTypes
import Parser (topParse)

type Result a = Either String a

topEval :: String -> Result Value
topEval = topParse >=> baseEval

-- Evaluate an expression in the empty environment and the empty store,
-- then discard the final state of the store and just return the value.
baseEval :: ExprC -> Result Value
baseEval = (`evalState` emptyStore) . eval emptyEnvironment

eval :: Environment -> ExprC -> StoreOp (Result Value)
eval _ (ValueC v) = return $ Right v
eval env (BinopC op l r) = do
    lval <- eval env l
    rval <- eval env r
    return $ join $ liftM2 (applyBinop op) lval rval
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
eval _ _ = return $ Left "not yet implemented"

typeError :: Show a => String -> String -> a -> Result b
typeError expected place actual = Left $ concat
    [ "type error: "
    , "expected ", expected, " in ", place, ", "
    , "but found: ", show actual
    ]
