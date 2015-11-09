module CoreTypes where

import qualified Data.Map as Map

type NumericValue = Int

data Value = NumV NumericValue
           | BoolV Bool
           | ClosureV [Identifier] ExprC Environment
           deriving (Eq, Show)

type Binop = Value -> Value -> Either String Value

type Identifier = String

data ExprC = ValueC Value
           | BinopC Identifier ExprC ExprC
           | IdC Identifier
           | LambdaC [Identifier] ExprC
           | AppC ExprC [ExprC]
           | IfC ExprC ExprC ExprC
           deriving (Eq, Show)

type Environment = Map.Map Identifier Value

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

envBind :: Identifier -> Value -> Environment -> Environment
envBind = Map.insert

envLookup :: Identifier -> Environment -> Maybe Value
envLookup = Map.lookup

-- Convert a value to a string to be printed.
-- Note that you can also just `show` the value,
-- which will retain more information
-- (e.g., NumV 123 will become "NumV 123" instead of just "123",
-- and closures will contain the entire closure body and environment.)
serialize :: Value -> String
serialize (NumV n) = show n
serialize (BoolV b) = if b then "true" else "false"
serialize ClosureV{} = "#<procedure>"
