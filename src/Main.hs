-- Simple entry point around the interpreter.
-- Reads a full program from stdin,
-- then parses and evaluates it,
-- printing the result to stdout or an error to stderr.
module Main(main) where

import System.Exit
import System.IO

import CoreTypes
import Interpreter (topEval)

printResult :: Either String Value -> IO ()
printResult (Left err) = hPutStrLn stderr err >> exitFailure
printResult (Right val) = print val >> exitSuccess

main :: IO ()
main = getContents >>= (printResult . topEval)
