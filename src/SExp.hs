-- Representation of s-expressions as an ADT,
-- along with a parser for them.
module SExp(SExp(Symbol, Number, List), sexp, parseSexp) where

import Text.Parsec
import Data.Char
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Number as PNumber

import Control.Monad

data SExp = Symbol String
          | Number Int
          | List [SExp]
          deriving (Show, Eq)

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
