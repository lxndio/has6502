module Utils where

import Data.Char (toUpper)

import Types

splitString :: Char -> String -> [String]
splitString c as = split c as "" where
  split :: Char -> String -> String -> [String]
  split c (a:as) curr
    | a == c          = if curr == "" then split c as "" else curr : (split c as "")
    | otherwise       = split c as (curr ++ [a])
  split c []     curr = if curr == "" then [] else [curr]

-- Removes whitespaces at the beginning and end of a string
trim :: String -> String
trim l = trimTail $ dropWhile (== ' ') l where
  trimTail ls = if l == ' ' then trimTail (init ls) else ls where
    l = last ls

-- Split string using spaces as delimiter and capitalize all characters
tokenizeString :: String -> TokenizedString
tokenizeString s = map (map toUpper) $ splitString ' ' s

-- Checks if a function is true for each character of a string,
-- returns false if it is not
parseString :: String -> (Char -> Bool) -> Bool
parseString (a:as) f = if f a then parseString as f else False
parseString []     _ = True

-- Returns the first n elements of a list
initN :: [a] -> Int -> [a]
initN s@(a:as) n
  | n > length as = s
  | n > 0         = a : initN as (n-1)
  | otherwise     = []

-- Returns the last n elements of a list
tailN :: [a] -> Int -> [a]
tailN as n
  | n < length as = loop as $ length as -1-n
  | otherwise     = as
  where
    loop (a:as) n
      | n > 0      = loop as $ n-1
      | otherwise  = as

-- Checks if a tokenized line is empty (apart from a label)
--isLineEmpty :: [String] -> Bool
--isLineEmpty l = (length l == 1) && (isLabel $ head l)

-- Get the first occurrence of a label in a tokenized line
--getLabel :: [String] -> Maybe String
--getLabel l = if length (labelList l) == 0 then Nothing else Just $ init $ head $ labelList l where
--  labelList l = filter ((== ':') . last) l

-- Add a specific line number to each parse error in a ParseErrors list
addLineNumbers :: ParseErrors -> Int -> ParseErrors
addLineNumbers (pe:pes) lineNr = ParseError lineNr (text pe) : addLineNumbers pes lineNr
addLineNumbers []       _      = []