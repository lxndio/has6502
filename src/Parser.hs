module Parser
( parseLines ) where

import Instructions
import Opcodes (generateOpcodes)
import Parameters (validateParameters)
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
generateLine lbl (Just instr) (prgCnt, lblLst) _      = Right $ (Line lbl $ Just "TODO", prgCnt+0, addLbl lbl prgCnt lblLst) -- TODO "TODO" and pgtCnt+0
  where
    addLbl :: Maybe String -> Int -> [(String, Int)] -> [(String, Int)]
    addLbl (Just lbl) prgCnt lblLst = (lbl, prgCnt) : lblLst
    addLbl Nothing    _      _      = lblLst

parseLine :: String -> (Int, [(String, Int)]) -> Int -> Either ParseError (Line, Int, [(String, Int)])
parseLine l env lineNr = generateLine (getLabel tl) (getInstruction' tl) env lineNr where
  tl = tokenizeString l

-- Same as parse line but takes a full env, that is env with an additional list of parsed lines
parseLineFullEnv :: String -> ([Line], Int, [(String, Int)]) -> Int -> Either ParseError ([Line], Int, [(String, Int)])
parseLineFullEnv l (ls, prgCnt, lblLst) lineNr = case parseLine l (prgCnt, lblLst) lineNr of
  Left pe -> Left pe
  Right (l', prgCnt', lblLst') -> Right (l' : ls, prgCnt', lblLst')

parseLines :: [String] -> Either ParseError ([Line], Int, [(String, Int)])
parseLines ls = parseLines' ls ([], 0, []) 1 where
  parseLines' :: [String] -> ([Line], Int, [(String, Int)]) -> Int -> Either ParseError ([Line], Int, [(String, Int)])
  parseLines' (l:ls) fullEnv lineNr = case parseLineFullEnv l fullEnv lineNr of
    Left pe       -> Left pe
    Right fullEnv -> parseLines' ls fullEnv (lineNr+1)
  parseLines' [] (ls, prgCnt, lblLst) _ = Right (reverse ls, prgCnt, lblLst) -- reverse because of ":" in parseLineFullEnv

--parseLines :: [String] -> [Line]
--parseLines (l:ls) = parseLine l : parseLines ls
--parseLines []     = []