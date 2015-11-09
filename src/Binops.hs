module Binops(binops, applyBinop) where

import CoreTypes

typeError :: String -> Either String b
typeError = Left . ("type error: " ++)

numericBinop :: (NumericValue -> NumericValue -> NumericValue) -> Binop
numericBinop op (NumV a) (NumV b) = Right $ NumV (a `op` b)
numericBinop _ _ _ = typeError "expected both operands to be numeric values"

binopDivide :: Value -> Value -> Either String Value
binopDivide (NumV _) (NumV 0) = Left "division by zero"
binopDivide (NumV a) (NumV b) = Right $ NumV $ a `div` b
binopDivide _ _ = typeError "expected both operands to be numeric values"

binopEqHuh :: Value -> Value -> Either String Value
binopEqHuh (NumV a) (NumV b)    = Right $ BoolV $ a == b
binopEqHuh (BoolV a) (BoolV b)  = Right $ BoolV $ a == b
binopEqHuh _ _                  = Right $ BoolV False

binops :: [(Identifier, Binop)]
binops = [ ("+", numericBinop (+))
         , ("-", numericBinop (-))
         , ("*", numericBinop (*))
         , ("/", binopDivide)
         , ("eq?", binopEqHuh)
         ]

applyBinop :: Identifier -> Value -> Value -> Either String Value
applyBinop name left right = case lookup name binops of
    Just op -> op left right
    Nothing -> Left "no such binary operator"
