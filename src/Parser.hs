{-# LANGUAGE LambdaCase #-}

-- Parse an s-expression into a LOOI program.
module Parser (parse, topParse) where

import Data.Maybe (isJust)
import qualified Data.Set as Set

import Control.Monad

import Binops (binops)
import CoreTypes
import SExp


type Result a = Either String a
type ParseResult = Result ExprC

--------------------------------------------------------------
-- Top-level parsing
--------------------------------------------------------------

-- Parse the given expression into an s-expression and then an AST.
topParse :: String -> ParseResult
topParse = parseQexpRaw >=> ensureNoQuasiquote >=> parse

-- We'll use quasiquotation ourselves in the desugaring process,
-- but we don't want the user input to be allowed to contain quasiquotation.
-- We'll check this up front and complain immediately if it's violated.
ensureNoQuasiquote :: QExp -> Result SExp
ensureNoQuasiquote q = case resolveQuasiquote ([], []) q of
    -- We take advantage of the fact that
    -- the `resolveQuasiquote` function, when passed the empty binding set,
    -- will throw an error exactly when the input actually uses quasiquotation.
    Right s -> Right s
    Left _ -> Left "quasiquotation is not allowed in LOOI programs"


--------------------------------------------------------------
-- S-expression-to-AST parsing
--------------------------------------------------------------

-- Parse an expression in the LOOI language to its AST,
-- or yield an error message describing what went wrong.
parse :: SExp -> ParseResult
--
-- num, true, false (ValueC)
parse (Number n) = Right $ ValueC $ NumV n
parse (Symbol "true") = Right $ ValueC $ BoolV True
parse (Symbol "false") = Right $ ValueC $ BoolV False
--
-- strings (not supported in this version of LOOI)
parse (String _) = Left "string literals are not supported in LOOI"
--
-- id (IdC)
parse (Symbol x) = IdC <$> ensureId x
--
-- {func id ... Expr} (LambdaC)
parse (List [Symbol "func"]) = Left "function definition is missing a body"
parse (List (Symbol "func" : xs)) = do
    let paramNames = init xs
    paramNames <- forM paramNames $ \case
        (Symbol name) -> ensureId name
        _ -> Left "expected lambda parameter to be a symbol"
    body <- parse $ last xs
    return $ LambdaC paramNames body
--
-- {with {id = Expr} ... Expr}
parse (List [Symbol "with"]) = Left "empty `with'-expression"
parse (List (Symbol "with" : args)) = do
    clauses <- mapM sugarWithClause $ init args
    let body = last args
    desugarWith clauses body
    where sugarWithClause (List [ id@(Symbol _)
                                , Symbol "="
                                , expr
                                ]) = Right (id, expr)
          sugarWithClause _ = Left "malformed clause in `with'-statement"
--
-- {if Expr Expr Expr} (IfC)
parse (List (Symbol "if" : args)) = parseIf args
--
-- {Expr ... Expr} (AppC or BinopC)
parse (List (target@(Symbol name):operands))
    | isBinopName name  = parseBinop name operands
    | otherwise         = parseApplication target operands
parse (List (target:operands)) = parseApplication target operands
--
parse (List []) = Left $ concat [ "empty application: "
                                , "you must provide a function expression "
                                , "or binary operator and operands"
                                ]

-- Parse a binary operator.
-- The identifier is assumed to refer to valid operator.
parseBinop :: Identifier -> [SExp] -> ParseResult
parseBinop opName args = do
    ensureArity 2 ("binary operator `" ++ opName ++ "'") args
    [arg1, arg2] <- mapM parse args
    return $ BinopC opName arg1 arg2

-- Desugar and parse a `with'-expression (local variable binding).
desugarWith :: [(SExp, SExp)] -> SExp -> ParseResult
desugarWith clauses body = parseQexp qbindings qexp >>= parse
    where spliceBindings = [ ("ids", map fst clauses)
                           , ("values", map snd clauses)
                           ]
          quoteBindings = [("body", body)]
          qbindings = (quoteBindings, spliceBindings)
          qexp = "{{func ,@ids ,body} ,@values}"

-- Parse a conditional expression.
parseIf :: [SExp] -> ParseResult
parseIf args = do
    ensureArity 3 "`if'-expression" args
    [guard, true, false] <- mapM parse args
    return $ IfC guard true false

-- Parse a function application.
parseApplication :: SExp -> [SExp] -> ParseResult
parseApplication target args = do
    targetExpr <- parse target
    argsExprs <- mapM parse args
    return $ AppC targetExpr argsExprs

-- Make sure a string represents a valid identifier;
-- return an error message if it doesn't.
ensureId :: Identifier -> Result Identifier
ensureId x
    | isBinopName x     = Left $ errmsg "a binary operator"
    | isReservedWord x  = Left $ errmsg "a reserved word"
    | otherwise         = Right x
    where errmsg why = "illegal identifier: " ++ show x ++ " is " ++ why

-- Ensure that the list of operands or arguments has the right length,
-- or raise an error if this it's wrong.
ensureArity :: Int -> String -> [a] -> Result [a]
ensureArity n name xs
    | length xs == n    = Right xs
    | otherwise         = Left $ concat
        [ "wrong arity to " , name, ": "
        , "expected " , show n, ", "
        , "but got " , show $ length xs
        ]

-- Determine whether the given identifier refers to a binary operator.
isBinopName :: Identifier -> Bool
isBinopName = isJust . flip lookup binops

-- Determine whether the given identifier is a reserved word.
isReservedWord :: Identifier -> Bool
isReservedWord = flip elem reserved
  where reserved = ["true", "false", "with", "if", "func"]
