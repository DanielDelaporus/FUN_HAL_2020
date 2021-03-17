module Main where

import Exec

import System.Environment
import System.Exit

printVar :: Var -> IO()
printVar (Var name value) = putStrLn (name ++ "=" ++ value)

printVars :: [Var] -> IO()
printVars (c:s) = printVar c
printVars [] = putStr ""

readAllFile :: [String] -> IO String
readAllFile (a:s) = do
    f1 <- readFile a
    f2 <- readAllFile s
    return (f1 ++ "\n" ++ f2)
readAllFile [] = return ""

main :: IO ()
main = do
  args <- getArgs
  case ((length args) > 0) of
    False ->  exitWith (ExitFailure 84)
    True -> do
      f <- readAllFile args
      executeAllFile f []-- to change
  exitWith (ExitSuccess)
