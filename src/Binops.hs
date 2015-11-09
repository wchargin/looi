module Binops(binops, applyBinop) where

import CoreTypes

-- A table mapping identifiers to binary operators.
-- You can use Prelude.lookup on this table to find a binop.
binops :: [(Identifier, Binop)]
binops = [ ("+", numericBinop (+))
         , ("-", numericBinop (-))
         , ("*", numericBinop (*))
         , ("/", divide)
         , ("eq?", eqHuh)
         , ("<=", leq)
         ]

-- Apply a binary operator of the given name to the given values.
-- Fail if the operator fails or if there is no such operator.
applyBinop :: Identifier -> Value -> Value -> Either String Value
applyBinop name left right = case lookup name binops of
    Just op -> op left right
    Nothing -> Left "no such binary operator"

-- Wrap an arithmetic expression in NumV typechecks and boxing/unboxing.
numericBinop :: (NumericValue -> NumericValue -> NumericValue) -> Binop
numericBinop op (NumV a) (NumV b) = Right $ NumV (a `op` b)
numericBinop _ _ _ = typeErrorExpectedNumeric

-- Perform normal division, but throw an error on division by zero.
divide :: Value -> Value -> Either String Value
divide (NumV _) (NumV 0) = Left "division by zero"
divide (NumV a) (NumV b) = Right $ NumV $ a `div` b
divide _ _ = typeErrorExpectedNumeric

-- Test whether two values are equal;
-- they are equal if they have the same value type and the same value,
-- except for procedures, which are never equal to anything.
--
-- It is not an error to invoke `eq?` with differently typed operands;
-- the result will simply be false.
eqHuh :: Value -> Value -> Either String Value
eqHuh (NumV a) (NumV b)    = Right $ BoolV $ a == b
eqHuh (BoolV a) (BoolV b)  = Right $ BoolV $ a == b
eqHuh _ _                  = Right $ BoolV False

-- Test whether the left number is less than or equal to the right number.
-- Throw an exception if either operand is not numeric.
leq :: Value -> Value -> Either String Value
leq (NumV a) (NumV b) = Right $ BoolV $ a <= b
leq _ _               = typeErrorExpectedNumeric

-- Throw an exception indicating that we expected numeric operands.
typeErrorExpectedNumeric :: Either String b
typeErrorExpectedNumeric = Left msg
    where msg = "type error: expected both operands to be numeric values"
