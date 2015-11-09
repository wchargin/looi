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

{-# ANN module "HLint: ignore Use String" #-}

--------------------------------------------------------------
-- Data definitions
--------------------------------------------------------------

data SExp = Symbol String
          | Number Int
          | List [SExp]
          deriving (Show, Eq)

data QExp = QSymbol String
          | QNumber Int
          | QList [QExp]
          | QUnquote UnquoteIdentifier
          | QSplice UnquoteIdentifier
          deriving (Show, Eq)

-- When converting a quasiquoted expression to an s-expression,
-- we need to substitute in existing s-expressions
-- at every QUnquote and QSplice.
-- In the concrete syntax, these are specified by name,
-- and then we look them up in a user-provided table;
-- each such name is an UnquoteIdentifier.
type UnquoteIdentifier = String

type UnquoteBindings = [(UnquoteIdentifier, SExp)]
type SplicingBindings = [(UnquoteIdentifier, [SExp])]
type QuasiquoteBindings = (UnquoteBindings, SplicingBindings)


--------------------------------------------------------------
-- Constants and useful parser combinators
--------------------------------------------------------------

-- Signals the beginning of a QUnquote or QSplice.
unquoteToken :: Char
unquoteToken = ','

-- Signals the beginning of a QSplice
-- when immediately following the unquoteToken.
splicingToken :: Char
splicingToken = '@'

-- Pairs of opening and closing delimiters
-- that can all be synonymously used for lists.
delimiters :: [(Char, Char)]
delimiters = zip "([{" ")]}"

-- A list of all delimiters
-- (useful for exclusion from the set of valid identifier characters, e.g.).
flatDelimiters :: [Char]
flatDelimiters = (uncurry (++) . unzip) delimiters

-- Match any character except for a space or one of the given characters.
exceptSpaceOr :: [Char] -> GenParser Char () Char
exceptSpaceOr xs = satisfy (\x -> not $ isSpace x || x `elem` xs)


--------------------------------------------------------------
-- Converting quasiquoted expressions to s-expressions
--------------------------------------------------------------

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


--------------------------------------------------------------
-- Parsers for quasiquoted expressions
--------------------------------------------------------------

-- A list, delimited by parentheses, brackets, or braces,
-- and containing zero or more sub-expressions.
listExpr :: GenParser Char () QExp
listExpr = foldl1 (<|>) $ flip map delimiters $
  \(open, close) -> between (char open) (char close) (fmap QList qexprs)

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
-- and not starting with a digit or unquote token.
symbolExpr :: GenParser Char () QExp
symbolExpr = QSymbol <$> liftM2 (:) initial (many internal)
  where nonWordChars = unquoteToken:flatDelimiters
        digits = ['0'..'9']
        initial = exceptSpaceOr $ nonWordChars ++ digits
        internal = exceptSpaceOr nonWordChars

-- The template name of an expression to be unquoted.
unquoteName :: GenParser Char () UnquoteIdentifier
unquoteName = many1 $ exceptSpaceOr $ unquoteToken:flatDelimiters

-- An unquote-splicing (,@xs) expression.
splicingExpr :: GenParser Char () QExp
splicingExpr = try (string ",@") >> spaces >> fmap QSplice unquoteName

-- An unquote (,x) expression.
unquoteExpr :: GenParser Char () QExp
unquoteExpr = char ',' >> spaces >> fmap QUnquote unquoteName

-- Either an unquote-splicing or unquote expression.
someKindOfUnquoteExpr :: GenParser Char () QExp
--
-- note: splicing has to come first so that we parse, e.g., ",@x"
-- as QSplice "x" and not QUnquote "@x"
someKindOfUnquoteExpr = splicingExpr <|> unquoteExpr

-- A positive or negative integer literal.
numberExpr :: GenParser Char () QExp
numberExpr = fmap QNumber PNumber.int

-- An arbitrary quasiquoted expression.
qexp :: GenParser Char () QExp
qexp = do
  spaces
  x <- someKindOfUnquoteExpr <|> listExpr <|> try numberExpr <|> symbolExpr
  spaces
  return x

-- The top-level quasiquoted expression,
-- which must be terminated by the end of input.
topExpr :: GenParser Char () QExp
topExpr = qexp >>= \x -> eof >> return x

--------------------------------------------------------------
-- The public interface
--------------------------------------------------------------

-- Unfortunately, the parse errors have different types from our errors,
-- so we lose a bit of context if it's a ParseError
-- (we only get the message, not the type).
castError :: Either ParseError a -> Either String a
castError (Right result) = Right result
castError (Left pe) = Left $ show pe

-- Parse a given string to a quasiquoted expression,
-- or return a parse error message.
parseQexpRaw :: String -> Either String QExp
parseQexpRaw = castError . parse topExpr ""

-- Parse a given string to a quasiquoted expression,
-- then resolve it to an s-expression.
-- If either step fails, yield an error message.
--
parseQexp :: QuasiquoteBindings -> String -> Either String SExp
parseQexp bindings input = parseQexpRaw input >>= resolveQuasiquote bindings

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
    Left err -> Left err
