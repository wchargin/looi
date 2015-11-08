module CoreTypes where

import qualified Data.Map as Map

type NumericValue = Int

data Value = NumV NumericValue
           | ClosureV Identifier ExprC Environment  -- unary-only for now
           deriving (Eq, Show)

type Binop = Value -> Value -> Either String Value

type Identifier = String

data ExprC = ValueC Value
           | BinopC Identifier ExprC ExprC
           | IdC Identifier
           | LambdaC Identifier ExprC               -- unary-only for now
           | AppC ExprC ExprC                       -- unary-only for now
           deriving (Eq, Show)

type Environment = Map.Map Identifier Value

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

envBind :: Identifier -> Value -> Environment -> Environment
envBind = Map.insert

envLookup :: Identifier -> Environment -> Maybe Value
envLookup = Map.lookup
