module CoreTypes where

import Control.Monad.State (State, gets, modify)
import Control.Monad.Except (Except)

import qualified Data.Map as Map

type NumericValue = Int

data Value = NumV NumericValue
           | BoolV Bool
           | ArrayV Address Int                         -- start, length
           | ClosureV [Identifier] ExprC Environment
           deriving (Eq, Show)

type Binop = Value -> Value -> Except String Value

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

type Address = Int
type Store = Map.Map Address Value
type StoreOp α = State Store α

emptyStore :: Store
emptyStore = Map.empty

storeLookup :: Address -> StoreOp (Maybe Value)
storeLookup a = gets $ Map.lookup a

allocate :: Value -> StoreOp Address
allocate v = do
    addr <- gets Map.size
    modify $ Map.insert addr v
    return addr

-- Convert a value to a string to be printed.
-- Note that you can also just `show` the value,
-- which will retain more information
-- (e.g., NumV 123 will become "NumV 123" instead of just "123",
-- and closures will contain the entire closure body and environment.)
serialize :: Value -> String
serialize (NumV n) = show n
serialize (BoolV b) = if b then "true" else "false"
serialize (ArrayV _ _) = "#<array>"
serialize ClosureV{} = "#<procedure>"
