-- Parse an s-expression into a LOOI program.
module Parser (parse) where

import Data.Maybe (isJust)
import qualified Data.Set as Set

import Control.Monad

import Binops (binops)
import CoreTypes
import SExp

-- Parse an expression in the LOOI language to its AST,
-- or yield an error message describing what went wrong.
parse :: SExp -> Either String ExprC
parse (Number n) = Right $ ValueC $ NumV n
parse (Symbol "true") = Right $ ValueC $ BoolV True
parse (Symbol "false") = Right $ ValueC $ BoolV False
parse (Symbol x) = ensureIdC x
parse (List [Symbol "func"]) = Left "function definition is missing a body"
parse (List (Symbol "func" : xs)) = do
    let paramNames = init xs
    paramNames <- forM paramNames $ \name -> case name of
        (Symbol name) -> ensureId name
        _ -> Left "expected lambda parameter to be a symbol"
    body <- parse $ last xs
    return $ LambdaC paramNames body
parse (List (target@(Symbol name):operands))
    | isBinopName name  = parseBinop name operands
    | otherwise         = parseApplication target operands
parse (List (target:operands)) = parseApplication target operands
parse (List []) = Left $ concat [ "empty application: "
                                , "you must provide a function expression "
                                , "or binary operator and operands"
                                ]

-- Parse a binary operator.
-- The identifier is assumed to refer to valid operator.
parseBinop :: Identifier -> [SExp] -> Either String ExprC
parseBinop opName args
    | length args == 2  = do [arg1, arg2] <- mapM parse args
                             return $ BinopC opName arg1 arg2
    | otherwise         = Left $ concat [ "wrong arity to binary operator: "
                                        , "expected 2, but got "
                                        , show $ length args
                                        ]

-- Parse a function application.
parseApplication :: SExp -> [SExp] -> Either String ExprC
parseApplication target args = do
    targetExpr <- parse target
    argsExprs <- mapM parse args
    return $ AppC targetExpr argsExprs

-- Make sure a string represents a valid identifier;
-- return an error message if it doesn't.
ensureId :: Identifier -> Either String Identifier
ensureId x
    | isBinopName x     = Left $ errmsg "a binary operator"
    | isReservedWord x  = Left $ errmsg "a reserved word"
    | otherwise         = Right x
    where errmsg why = "illegal identifier: " ++ show x ++ " is " ++ why

-- Make sure a string represents a valid identifier;
-- then wrap it in an IdC.
ensureIdC :: Identifier -> Either String ExprC
ensureIdC = fmap IdC . ensureId

-- Determine whether the given identifier refers to a binary operator.
isBinopName :: Identifier -> Bool
isBinopName = isJust . flip lookup binops

-- Determine whether the given identifier is a reserved word.
isReservedWord :: Identifier -> Bool
isReservedWord = flip elem reserved
  where reserved = ["true", "false", "with", "if", "func"]
