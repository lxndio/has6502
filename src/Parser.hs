module Parser
( parseLine ) where

import Data.Char (toUpper)

data Label = Label String deriving (Show)

data Instruction = Instruction { name :: String
                               , opcode :: String
                               } deriving (Show)

data Line = Line { label  :: Maybe Label
                 , instr  :: Maybe Instruction
                 , params :: Maybe [String]
                 } deriving (Show)

data ParseError = ParseError { line :: Int
                             , msg  :: String
                             } deriving (Show)

instructions :: [Instruction]
instructions = [Instruction "LDA" ""]

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

maybeHead :: [a] -> Maybe a
maybeHead as = if length as == 0 then Nothing else Just $ head as

-- Get the first occurrence of a label in a tokenized line
getLabel :: [String] -> Maybe Label
getLabel l = if length (labelList l) == 0 then Nothing else Just $ Label $ init $ head $ labelList l where
  labelList l = filter ((== ':') . last) l

getInstrByName :: String -> Maybe Instruction
getInstrByName s = if length instructions' == 0 then Nothing else Just $ head $ instructions' where
  instructions' = filter ((== s) . name) instructions

instrExistsByName :: String -> Bool
instrExistsByName s = if length instructions' == 0 then False else True where
  instructions' = filter ((== s) . name) instructions

-- Get the first occurrence of an instruction in a tokenized line
getInstruction :: [String] -> Maybe Instruction
getInstruction (l:ls) = case getInstrByName l of i@(Just _) -> i
                                                 Nothing    -> getInstruction ls
getInstruction []     = Nothing

-- TODO Use monad to throw ParseError on failure caused by nothing in case of:
--      - no instruction but parameters
--      - parameters but no instruction
--      - non-label text before instruction
--      - etc.
parseLine :: String -> Either ParseError Line
parseLine l = Right Line { label = getLabel tl
                         , instr = getInstruction tl
                         , params = Nothing
                         } where
  tl = tokenizeString l


--parseLines :: [String] -> [Line]
--parseLines (l:ls) = parseLine l : parseLines ls
--parseLines []     = []