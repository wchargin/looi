-- Simple entry point around the interpreter.
-- Reads a full program from stdin,
-- then parses and evaluates it,
-- printing the result to stdout or an error to stderr.
module Main(main) where

import System.Exit
import System.IO

import Control.Monad.Except (runExcept)

import CoreTypes
import Interpreter (topEval)

printResult :: Either String Value -> IO ()
printResult (Left err) = hPutStrLn stderr err >> exitFailure
printResult (Right val) = putStrLn (serialize val) >> exitSuccess

main :: IO ()
main = getContents >>= (printResult . runExcept . topEval)
