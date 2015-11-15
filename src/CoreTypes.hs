{-# LANGUAGE FlexibleContexts, TupleSections #-}

module CoreTypes where

import Control.Monad.State (State, gets, modify)
import Control.Monad.Except (Except)
import Control.Monad.State.Class (MonadState, get, gets, put)

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

--------------------------------------------------------------
-- The environment
--------------------------------------------------------------

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
serialize (ArrayV _ _) = "#<array>"
serialize ClosureV{} = "#<procedure>"


--------------------------------------------------------------
-- The store
--------------------------------------------------------------

-- Look up a value in the store based on its address.
-- Return Nothing if the specified address is not allocated.
storeLookup :: MonadState Store m => Address -> m (Maybe Value)
storeLookup addr = gets $ Map.lookup addr . contents

type Address = Int
data Store = Store { contents :: Map.Map Address Value
                   , nextAddress :: Address
                   }
                   deriving (Show, Eq)
type StoreOp α = State Store α

emptyStore :: Store
emptyStore = Store Map.empty 0

-- Allocate a bunch of blocks in the store,
-- initializing each one to the given value,
-- and return the starting address.
allocateMany :: MonadState Store m => Int -> Value -> m Address
allocateMany num val = do
    Store old addr <- get
    let new = Map.union (Map.fromList $ map (, val) $ take num [addr..]) old
    put $ Store new (addr + num)
    return addr

-- Allocate a single block in the store with the given value,
-- and return its new address.
allocate :: MonadState Store m => Value -> m Address
allocate = allocateMany 1
