module CoreTypes where

import qualified Data.Map as Map

type NumericValue = Int

data Value = NumV NumericValue
           | BoolV Bool
           | ClosureV Identifier [ExprC] Environment
           deriving (Eq, Show)

type Binop = Value -> Value -> Either String Value

type Identifier = String

data ExprC = ValueC Value
           | BinopC Identifier ExprC ExprC
           | IdC Identifier
           | LambdaC [Identifier] ExprC
           | AppC ExprC [ExprC]
           deriving (Eq, Show)

type Environment = Map.Map Identifier Value

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

envBind :: Identifier -> Value -> Environment -> Environment
envBind = Map.insert

envLookup :: Identifier -> Environment -> Maybe Value
envLookup = Map.lookup
