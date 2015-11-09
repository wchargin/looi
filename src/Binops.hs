module Binops(binops, applyBinop) where

import CoreTypes

typeError :: String -> Either String b
typeError = Left . ("type error: " ++)

numericBinop :: (NumericValue -> NumericValue -> NumericValue) -> Binop
numericBinop op (NumV a) (NumV b) = Right $ NumV (a `op` b)
numericBinop _ _ _ = typeError "expected both operands to be numeric values"

divide :: Value -> Value -> Either String Value
divide (NumV _) (NumV 0) = Left "division by zero"
divide (NumV a) (NumV b) = Right $ NumV $ a `div` b
divide _ _ = typeError "expected both operands to be numeric values"

eqHuh :: Value -> Value -> Either String Value
eqHuh (NumV a) (NumV b)    = Right $ BoolV $ a == b
eqHuh (BoolV a) (BoolV b)  = Right $ BoolV $ a == b
eqHuh _ _                  = Right $ BoolV False

leq :: Value -> Value -> Either String Value
leq (NumV a) (NumV b) = Right $ BoolV $ a <= b
leq _ _               = typeError "expected both operands to be numeric values"

binops :: [(Identifier, Binop)]
binops = [ ("+", numericBinop (+))
         , ("-", numericBinop (-))
         , ("*", numericBinop (*))
         , ("/", divide)
         , ("eq?", eqHuh)
         , ("<=", leq)
         ]

applyBinop :: Identifier -> Value -> Value -> Either String Value
applyBinop name left right = case lookup name binops of
    Just op -> op left right
    Nothing -> Left "no such binary operator"
