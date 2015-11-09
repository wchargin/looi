-- Representation of s-expressions as an ADT,
-- along with a parser for them.
-- Includes support for quasiquoting.
module SExp ( SExp(..)
            , QExp(..)
            , parseSexp
            , parseQexp
            , parseQexpRaw
            , resolveQuasiquote
            ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Number as PNumber

import Control.Monad
import Data.Char (isSpace)

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
listExpr :: GenParser Char () QExp
listExpr = foldl1 (<|>) $ flip map (zip "([{" ")]}") $
  \(open, close) -> between (char open) (char close) (QList <$> qexprs)

-- Zero or more expressions, separated and bordered by arbitrary spaces.
qexprs :: GenParser Char () [QExp]
qexprs = do
  -- We need these `spaces` even though expressions trim their own spaces
  -- to account for the case where the list is empty but contains spaces.
  -- (We don't want to try to parse an expression just because we see a space.)
  spaces
  item <- optionMaybe qexp
  case item of
    Just x -> fmap (x:) qexprs
    Nothing -> return []

-- A symbol, containing any non-whitespace characters (not necessarily ASCII)
-- and not starting with a digit.
symbolExpr :: GenParser Char () QExp
symbolExpr = QSymbol <$> liftM2 (:) initial (many internal)
  where delimiters = "([{}])"
        templateToken = ','
        nonWordChars = templateToken:delimiters
        digits = ['0'..'9']
        except xs x = not $ isSpace x || x `elem` xs
        initial = satisfy $ except $ nonWordChars ++ digits
        internal = satisfy $ except nonWordChars

-- The template name of an expression to be unquoted.
unquoteName :: GenParser Char () UnquoteIdentifier
unquoteName = many1 $ satisfy pred
  where pred x = not $ isSpace x || x `elem` (templateToken:delimiters)
        delimiters = "([{}])"
        templateToken = ','

-- An unquote-splicing (,@xs) expression.
splicingExpr :: GenParser Char () QExp
splicingExpr = try (string ",@") >> spaces >> fmap QSplice unquoteName

-- An unquote (,x) expression.
unquoteExpr :: GenParser Char () QExp
unquoteExpr = char ',' >> spaces >> fmap QUnquote unquoteName

-- Either an unquote-splicing or unquote expression.
someKindOfUnquoteExpr :: GenParser Char () QExp
--
-- note: splicing has to come first so that
-- unquote doesn't parse ",@x" as QUnquote "@x"
someKindOfUnquoteExpr = splicingExpr <|> unquoteExpr

-- A positive or negative integer literal.
numberExpr :: GenParser Char () QExp
numberExpr = fmap QNumber PNumber.int

-- An arbitrary quasiquoted expression.
qexp :: GenParser Char () QExp
qexp = do
  spaces
  x <- someKindOfUnquoteExpr <|> listExpr <|> numberExpr <|> symbolExpr
  spaces
  return x

-- The top-level quasiquoted expression,
-- which must be terminated by the end of input.
topExpr :: GenParser Char () QExp
topExpr = qexp >>= \x -> eof >> return x

-- Parse a given string to a quasiquoted expression,
-- or return a parse error message.
parseQexpRaw :: String -> Either ParseError QExp
parseQexpRaw = parse topExpr ""

-- Parse a given string to a quasiquoted expression,
-- then resolve it to an s-expression.
-- If either step fails, yield an error message.
--
-- Unfortunately, the errors have different types,
-- so we lose a bit of context if it's a ParseError
-- (we only get the message, not the type).
-- If you care about that, use resolveQuasiquote and handle it yourself.
parseQexp :: QuasiquoteBindings -> String -> Either String SExp
parseQexp bindings input = case parseQexpRaw input of
    Right qexp -> resolveQuasiquote bindings qexp
    Left pe -> Left $ show pe

-- Parse a given string to an s-expression.
-- If the string contains quasiquoted parts, it will throw an error.
parseSexp :: String -> Either String SExp
parseSexp input = case parseQexpRaw input of
    Right qexp -> case resolveQuasiquote ([], []) qexp of
        Right sexp -> Right sexp
        Left err -> Left $ concat
            [ "detected use of quasiquote in s-expression; "
            , "did you mean to use parseQexp and provide bindings? "
            , "(original error: "
            , err
            , ")"
            ]
    Left pe -> Left $ show pe
