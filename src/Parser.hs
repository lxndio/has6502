module Parser
( parseLine ) where

data Line = Line { label  :: String
                 , instr  :: String
                 , params :: [String]
                 } deriving (Show)

instructions :: [String]
instructions = ["LDA"]

splitString :: Char -> String -> [String]
splitString c as = split c as "" where
  split :: Char -> String -> String -> [String]
  split c (a:as) curr
    | a == c          = curr : (split c as "")
    | otherwise       = split c as (curr ++ [a])
  split c []     curr = [curr]

-- Gets the position of the first occurrence of a char in a string
posOfChar :: Char -> String -> Int
posOfChar c as = countPos c as 0 where
  countPos :: Char -> String -> Int -> Int
  countPos c (a:as) pos
    | a == c            = pos
    | otherwise         = countPos c as (pos+1)

-- Removes whitespaces at the beginning and end of a string
trim :: String -> String
trim l = trimTail $ dropWhile (== ' ') l where
  trimTail ls = if l == ' ' then trimTail (init ls) else ls where
    l = last ls

hasLabel :: String -> Bool
hasLabel l = (':' `elem` l) && (l !! (posOfChar ':' l - 1) /= ' ')

getLabel :: String -> Maybe String
getLabel l = if hasLabel l then Just (trim $ take (posOfChar ':' l) l) else Nothing

removeLabel :: String -> String
removeLabel l = if hasLabel l then drop (posOfChar ':' l + 1) l else l

parseLine :: String -> Line
parseLine l = Line { label = labelOfL
                   , instr = ""
                   , params = []
                   } where
  labelOfL = case getLabel l of Just label -> label
                                Nothing    -> ""

parseProgram :: [String] -> [Line]
parseProgram (l:ls) = parseLine l : parseProgram ls
parseProgram []     = []