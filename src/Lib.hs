module Lib where

printStr :: String -> IO ()
printStr  = putStrLn

type ProblemResult = (Integer, String)
type Results = [ProblemResult]


