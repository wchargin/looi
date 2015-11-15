{-# LANGUAGE LambdaCase #-}

module Interpreter (eval, topEval) where

import Control.Monad
import Control.Monad.Except (Except, runExcept, throwError)

import Binops (applyBinop)
import CoreTypes
import Parser (topParse)

type Result a = Except String a

topEval :: String -> Result Value
topEval = topParse >=> eval emptyEnvironment

eval :: Environment -> ExprC -> Result Value
eval _ (ValueC v) = return v
eval env (BinopC op l r) = do
    lval <- eval env l
    rval <- eval env r
    applyBinop op lval rval
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
eval _ _ = throwError "not yet implemented"

typeError :: Show a => String -> String -> a -> Result b
typeError expected place actual = throwError $ concat
    [ "type error: "
    , "expected ", expected, " in ", place, ", "
    , "but found: ", show actual
    ]
