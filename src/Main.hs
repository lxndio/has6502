module Main where

import Assembler
import Parser
import Types

printParseErrors :: ParseErrors -> IO ()
printParseErrors (pe:pes) = do
  putStrLn $ "Line " ++ show (line pe) ++ ":\t" ++ text pe
  printParseErrors pes
printParseErrors [] = return ()

main :: IO ()
main = do
  content <- readFile "test.asm"
  let contentLines = lines content
  case parseLines contentLines of
    Left pes -> printParseErrors pes
    Right _  -> putStrLn "Okay"