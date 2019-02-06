module Parser
( parseLine ) where

import Instructions
import Utils

data Line = Line { label  :: Maybe String
                 , bytes  :: Maybe String
                 } deriving (Show)

data ParseError = ParseError { line :: Int
                             , msg  :: String
                             } deriving (Show)

-- TODO Use monad to throw ParseError on failure caused by nothing in case of:
--      - no instruction but parameters
--      - parameters but no instruction
--      - non-label text before instruction
--      - anything before label
--      - etc.

-- Returns: Either ParseError (Line, prgCnt, lblLst)
generateLine :: Maybe String -> Maybe Instruction -> (Int, [(String, Int)]) -> Int -> Either ParseError (Line, Int, [(String, Int)])
generateLine _   Nothing      _                lineNr = Left  $ ParseError lineNr "unknown instruction"
generateLine lbl (Just instr) (prgCnt, lblLst) _      = Right $ (Line lbl $ Just "TODO", prgCnt+0, addLbl lbl prgCnt lblLst)
  where
    addLbl :: Maybe String -> Int -> [(String, Int)] -> [(String, Int)]
    addLbl (Just lbl) prgCnt lblLst = (lbl, prgCnt) : lblLst
    addLbl Nothing    _      _      = lblLst

parseLine :: String -> (Int, [(String, Int)]) -> Int -> Either ParseError (Line, Int, [(String, Int)])
parseLine l env lineNr = generateLine (getLabel tl) (getInstruction' tl) env lineNr where
  tl = tokenizeString l

--parseLines :: [String] -> [Line]
--parseLines (l:ls) = parseLine l : parseLines ls
--parseLines []     = []