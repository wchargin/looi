-- Representation of s-expressions as an ADT,
-- along with a parser for them.
-- Includes support for quasiquoting.
module SExp(SExp(..), QExp(..), sexp, parseSexp, resolveQuasiquote) where

import Text.Parsec
import Data.Char
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Number as PNumber

import Control.Monad

data SExp = Symbol String
          | Number Int
          | List [SExp]
          deriving (Show, Eq)

type UnquoteIdentifier = String

-- A quasi-quoted expression.
data QExp = QSymbol String
          | QNumber Int
          | QList [QExp]
          | QUnquote UnquoteIdentifier
          | QSplice UnquoteIdentifier
          deriving (Show, Eq)

type UnquoteBindings = [(UnquoteIdentifier, SExp)]
type SplicingBindings = [(UnquoteIdentifier, [SExp])]
type QuasiquoteBindings = (UnquoteBindings, SplicingBindings)

-- Expand a quasiquoted expression into zero or more s-expressions.
-- This is needed as a layer of indirection
-- because QSplice (unquote-splicing) terms
-- can contain any number of values.
expandQuasiquote :: QuasiquoteBindings -> QExp -> Either String [SExp]
expandQuasiquote _ (QSymbol s) = Right [Symbol s]
expandQuasiquote _ (QNumber n) = Right [Number n]
expandQuasiquote bs (QList xs) = do
    subresults <- mapM (expandQuasiquote bs) xs
    let listItems = concat subresults
    return [List listItems]
expandQuasiquote (ub, _) (QUnquote id) = case lookup id ub of
    Just sexp -> Right [sexp]
    Nothing -> Left $ "unbound unquote by the name of: `" ++ id ++ "'"
expandQuasiquote (_, sb) (QSplice id) = case lookup id sb of
    Just sexps -> Right sexps
    Nothing -> Left $ "unbound unquote-splicing by the name of: `" ++ id ++ "'"

resolveQuasiquote :: QuasiquoteBindings -> QExp -> Either String SExp
resolveQuasiquote _ (QSplice _) = Left
    "unquote-splicing is only allowed within a list, not at the top level"
resolveQuasiquote bs qexp = case expandQuasiquote bs qexp of
    Left err -> Left err
    Right [single] -> Right single
    Right _ -> Left "invariant violation: expected a single expanded SExp"

-- A list, delimited by parentheses, brackets, or braces,
-- and containing zero or more sub-expressions.
listExpr :: GenParser Char () SExp
listExpr = foldl1 (<|>) $ flip map (zip "([{" ")]}") $
  \(open, close) -> between (char open) (char close) (fmap List exprs)

-- Zero or more expressions, separated and bordered by arbitrary spaces.
exprs :: GenParser Char () [SExp]
exprs = do
  -- We need these `spaces` even though expressions trim their own spaces
  -- to account for the case where the list is empty but contains spaces.
  -- (We don't want to try to parse an expression just because we see a space.)
  spaces
  item <- optionMaybe sexp
  case item of
    Just x -> fmap (x:) exprs
    Nothing -> return []

-- A symbol, containing any non-whitespace characters (not necessarily ASCII)
-- and not starting with a digit.
symbolExpr :: GenParser Char () SExp
symbolExpr = Symbol <$> liftM2 (:) initial (many internal)
  where delimiters = "([{}])"
        digits = ['0'..'9']
        except xs x = not $ isSpace x || x `elem` xs
        initial = satisfy $ except $ delimiters ++ digits
        internal = satisfy $ except delimiters

-- A positive or negative integer literal.
numberExpr :: GenParser Char () SExp
numberExpr = fmap Number PNumber.int

-- An arbitrary s-expression.
sexp :: GenParser Char () SExp
sexp = do
  spaces
  x <- listExpr <|> numberExpr <|> symbolExpr
  spaces
  return x

-- The top-level s-expression, which must be terminated by the end of input.
topExpr :: GenParser Char () SExp
topExpr = sexp >>= \x -> eof >> return x

-- Parse a given string to an s-expression, or return a parse error message.
parseSexp :: String -> Either ParseError SExp
parseSexp = parse topExpr ""
