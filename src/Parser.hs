{-# LANGUAGE LambdaCase, FlexibleContexts #-}

-- Parse an s-expression into a LOOI program.
module Parser (parse, topParse) where

import Data.Maybe (isJust)
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Except (Except, withExcept, throwError)

import Binops (isBinopName)
import CoreTypes
import SExp


type Result a = Except String a
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
ensureNoQuasiquote q =
    -- We take advantage of the fact that
    -- the `resolveQuasiquote` function, when passed the empty binding set,
    -- will throw an error exactly when the input actually uses quasiquotation.
    withExcept
        (const "quasiquotation is not allowed in LOOI programs")
        (resolveQuasiquote ([], []) q)


--------------------------------------------------------------
-- S-expression-to-AST parsing
--------------------------------------------------------------

-- Parse an expression in the LOOI language to its AST,
-- or yield an error message describing what went wrong.
parse :: SExp -> ParseResult
--
-- num, true, false (ValueC)
parse (Number n) = return $ ValueC $ NumV n
parse (Symbol "true") = return $ ValueC $ BoolV True
parse (Symbol "false") = return $ ValueC $ BoolV False
--
-- strings (not supported in this version of LOOI)
parse (String _) = throwError "string literals are not supported in LOOI"
--
-- array initializers with length and value
parse (List (Symbol "new-array" : operands)) = parseNewArray operands
--
-- {id <- Expr} (SetC)
parse (List [Symbol id, Symbol "<-", expr]) = do
    id' <- ensureId id
    expr' <- parse expr
    return $ SetC id' expr'
--
-- {if Expr Expr Expr} (IfC)
parse (List (Symbol "if" : args)) = parseIf args
--
-- {begin Expr ...} (SeqC)
parse (List [Symbol "begin"]) = throwError "begin block must not be empty"
parse (List (Symbol "begin" : exprs)) = SeqC <$> mapM parse exprs
--
-- id (IdC)
parse (Symbol x) = IdC <$> ensureId x
--
-- {func id ... Expr} (LambdaC)
parse (List [Symbol "func"]) = throwError "function definition is missing a body"
parse (List (Symbol "func" : xs)) = do
    let paramNames = init xs
    paramNames <- forM paramNames $ \case
        (Symbol name) -> ensureId name
        _ -> throwError "expected lambda parameter to be a symbol"
    body <- parse $ last xs
    return $ LambdaC paramNames body
--
-- {with {id = Expr} ... Expr}
parse (List [Symbol "with"]) = throwError "empty `with'-expression"
parse (List (Symbol "with" : args)) = do
    clauses <- mapM sugarWithClause $ init args
    let body = last args
    desugarWith clauses body
  where
    sugarWithClause (List [id@(Symbol _), Symbol "=", x]) = return (id, x)
    sugarWithClause _ = throwError "malformed clause in `with'-statement"
--
-- {Expr ... Expr} (AppC or BinopC)
parse (List (target@(Symbol name):operands))
    | isBinopName name  = parseBinop name operands
    | otherwise         = parseApplication target operands
parse (List (target:operands)) = parseApplication target operands
--
parse (List []) = throwError $ concat
    [ "empty application: "
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

-- Parse an array initializer with length and initial value.
parseNewArray :: [SExp] -> ParseResult
parseNewArray args = do
    ensureArity 2 "`newArrayC' expression" args
    [len, val] <- mapM parse args
    return $ NewArrayC len val

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
    | isBinopName x     = throwError $ errmsg "a binary operator"
    | isReservedWord x  = throwError $ errmsg "a reserved word"
    | otherwise         = return x
    where errmsg why = "illegal identifier: " ++ show x ++ " is " ++ why

-- Ensure that the list of operands or arguments has the right length,
-- or raise an error if this it's wrong.
ensureArity :: Int -> String -> [a] -> Result [a]
ensureArity n name xs
    | length xs == n    = return xs
    | otherwise         = throwError $ concat
        [ "wrong arity to " , name, ": "
        , "expected " , show n, ", "
        , "but got " , show $ length xs
        ]

-- Determine whether the given identifier is a reserved word.
isReservedWord :: Identifier -> Bool
isReservedWord = flip elem reserved
  where
      reserved =
          [ "true"
          , "false"
          , "with"
          , "if"
          , "func"
          , "<-"
          , "new-array"
          ]
