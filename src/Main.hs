module Main where

import Parser

main :: IO ()
main = do
  content <- readFile "test.asm"
  let contentLines = lines content
  putStrLn $ show $ parseLines contentLines