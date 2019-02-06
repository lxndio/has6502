module Utils
( splitString
, trim
, tokenizeString
, isLineEmpty
, getLabel
, isLabel ) where

import Data.Char (toUpper)

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

-- Split string using space as delimiter and capitalize all characters
tokenizeString :: String -> [String]
tokenizeString s = map (map toUpper) $ splitString ' ' s

-- Checks if a tokenized line is empty (apart from a label)
isLineEmpty :: [String] -> Bool
isLineEmpty l = (length l == 1) && (isLabel $ head l)

-- Get the first occurrence of a label in a tokenized line
getLabel :: [String] -> Maybe String
getLabel l = if length (labelList l) == 0 then Nothing else Just $ init $ head $ labelList l where
  labelList l = filter ((== ':') . last) l

-- Checks if a given string has the format required to be label
isLabel :: String -> Bool
isLabel = (== ':') . last