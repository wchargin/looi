{-# LANGUAGE FlexibleContexts, LambdaCase #-}

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
import Control.Monad.Except (Except, throwError, withExcept)
import Data.Char (isSpace)

{-# ANN module "HLint: ignore Use String" #-}

--------------------------------------------------------------
-- Data definitions
--------------------------------------------------------------

data SExp = Symbol String
          | String String
          | Number Int
          | List [SExp]
          deriving (Show, Eq)

data QExp = QSymbol String
          | QString String
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

-- Match a parser, then another parser, then discard the second.
followedBy :: Monad m => m a -> m b -> m a
followedBy x y = x >>= \result -> y >> return result


--------------------------------------------------------------
-- Converting quasiquoted expressions to s-expressions
--------------------------------------------------------------

-- Expand a quasiquoted expression into zero or more s-expressions.
-- This is needed as a layer of indirection
-- because QSplice (unquote-splicing) terms
-- can contain any number of values.
expandQuasiquote :: QuasiquoteBindings -> QExp -> Except String [SExp]
expandQuasiquote _ (QSymbol s) = return [Symbol s]
expandQuasiquote _ (QString s) = return [String s]
expandQuasiquote _ (QNumber n) = return [Number n]
expandQuasiquote bs (QList xs) = do
    subresults <- mapM (expandQuasiquote bs) xs
    let listItems = concat subresults
    return [List listItems]
expandQuasiquote (ub, _) (QUnquote id) = case lookup id ub of
    Just sexp -> return [sexp]
    Nothing -> throwError $ "unbound unquote by the name of: `" ++ id ++ "'"
expandQuasiquote (_, sb) (QSplice id) = case lookup id sb of
    Just sexps -> return sexps
    Nothing -> throwError $
        "unbound unquote-splicing by the name of: `" ++ id ++ "'"

resolveQuasiquote :: QuasiquoteBindings -> QExp -> Except String SExp
resolveQuasiquote _ (QSplice _) = throwError
    "unquote-splicing is only allowed within a list, not at the top level"
resolveQuasiquote bs qexp = expandQuasiquote bs qexp >>= \case
    [single] -> return single
    _ -> throwError "invariant violation: expected a single expanded SExp"


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

-- A string, delimited by "quotes" and allowing escape sequences.
stringExpr :: GenParser Char () QExp
stringExpr = fmap QString $ char '"' >> many stringChar `followedBy` char '"'
    where stringChar = noneOf ['\\', '"'] <|> (char '\\' >> escapeSequence)
          escapeSequence = foldl1 (<|>) (map replace escapes)
          replace (old, new) = char old >> return new
          escapes = [ ('\\', '\\')
                    , ('"', '"')
                    , ('n', '\n')
                    , ('r', '\r')
                    , ('t', '\t')
                    ]

-- A symbol, containing any non-whitespace characters (not necessarily ASCII)
-- and not starting with a digit or unquote token.
symbolExpr :: GenParser Char () QExp
symbolExpr = QSymbol <$> liftM2 (:) initial (many internal)
  where nonWordChars = unquoteToken:flatDelimiters
        digits = ['0'..'9']
        initial = exceptSpaceOr $ nonWordChars ++ digits
        internal = exceptSpaceOr nonWordChars

-- Either an unquote-splicing or unquote expression.
unquoteExpr :: GenParser Char () QExp
unquoteExpr = char unquoteToken >> (splicing <|> unquote)
    --
    -- note: splicing has to come first so that we parse, e.g., ",@x"
    -- as QSplice "x" and not QUnquote "@x"
    where splicing = char splicingToken >> spaces >> fmap QSplice name
          unquote  = spaces >> fmap QUnquote name
          name     = many1 $ exceptSpaceOr $ unquoteToken:flatDelimiters

-- A positive or negative integer literal.
numberExpr :: GenParser Char () QExp
numberExpr = fmap QNumber PNumber.int

-- An arbitrary quasiquoted expression.
qexp :: GenParser Char () QExp
qexp = spaces >>
        (try unquoteExpr
         <|> listExpr
         <|> stringExpr
         <|> try numberExpr
         <|> symbolExpr) `followedBy` spaces

-- The top-level quasiquoted expression,
-- which must be terminated by the end of input.
topExpr :: GenParser Char () QExp
topExpr = qexp `followedBy` eof

--------------------------------------------------------------
-- The public interface
--------------------------------------------------------------

-- Unfortunately, the parse errors have different types from our errors,
-- so we lose a bit of context if it's a ParseError
-- (we only get the message, not the type).
castError :: Either ParseError a -> Except String a
castError (Right result) = return result
castError (Left pe) = throwError $ show pe

-- Parse a given string to a quasiquoted expression,
-- or return a parse error message.
parseQexpRaw :: String -> Except String QExp
parseQexpRaw = castError . parse topExpr ""

-- Parse a given string to a quasiquoted expression,
-- then resolve it to an s-expression.
-- If either step fails, yield an error message.
--
parseQexp :: QuasiquoteBindings -> String -> Except String SExp
parseQexp bindings input = parseQexpRaw input >>= resolveQuasiquote bindings

-- Parse a given string to an s-expression.
-- If the string contains quasiquoted parts, it will throw an error.
parseSexp input = do
    qexp <- parseQexpRaw input
    withExcept
        (\err -> concat
            [ "detected use of quasiquote in s-expression; "
            , "did you mean to use parseQexp and provide bindings? "
            , "(original error: "
            , err
            , ")"
            ])
        (resolveQuasiquote ([], []) qexp)
