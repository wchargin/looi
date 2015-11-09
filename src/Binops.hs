module Binops(binops, applyBinop) where

import CoreTypes

typeErrorExpectedNumeric :: Either String b
typeErrorExpectedNumeric = Left msg
    where msg = "type error: expected both operands to be numeric values"

numericBinop :: (NumericValue -> NumericValue -> NumericValue) -> Binop
numericBinop op (NumV a) (NumV b) = Right $ NumV (a `op` b)
numericBinop _ _ _ = typeErrorExpectedNumeric

divide :: Value -> Value -> Either String Value
divide (NumV _) (NumV 0) = Left "division by zero"
divide (NumV a) (NumV b) = Right $ NumV $ a `div` b
divide _ _ = typeErrorExpectedNumeric

eqHuh :: Value -> Value -> Either String Value
eqHuh (NumV a) (NumV b)    = Right $ BoolV $ a == b
eqHuh (BoolV a) (BoolV b)  = Right $ BoolV $ a == b
eqHuh _ _                  = Right $ BoolV False

leq :: Value -> Value -> Either String Value
leq (NumV a) (NumV b) = Right $ BoolV $ a <= b
leq _ _               = typeErrorExpectedNumeric

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
