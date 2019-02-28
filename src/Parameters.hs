module Parameters
( getParameters
, getParameterType
, getOpcodeByParamType ) where

import Data.Char (isLetter, isNumber)

import Hex (hexToInt, stringToHex, isHex)
import Instructions (instrExistsByName, getInstrByName')
import Parser (isLabel)
import Types
import Utils (parseString, initN, tailN)

-- Get all parameters from a tokenized line
getParameters :: [String] -> Maybe [String]
getParameters l = if length pl > 0 then Just pl else Nothing where
  pl = dropWhile cond l
  cond x = isLabel x || instrExistsByName x

-- Detect what parameter type a string represents
getParameterType :: String -> Environment -> ParameterType
getParameterType s@(a:as) env
  | s == ""                       = Implied
  | s == "A"                      = Accumulator
  | a == '#'                      = Immediate
  | parseString (noXY s) isNumber = if read (noXY s) > 255
      then case tailN s 2 of ",X" -> AbsoluteX
                             ",Y" -> AbsoluteY
                             _    -> Absolute
      else case tailN s 2 of ",X" -> ZeroPageX
                             ",Y" -> ZeroPageY
                             _    -> ZeroPage
  | (labelList env) `containsKey` (noXY s) = case getValue $ labelList env of
      Just address -> getParameterType (getValue (labelList env) (noXY s)) env
      Nothing      -> Unknown
  | a == '$' &&
    parseString (noXY as) isHex   = if (hexToInt $ stringToHex $ noXY as) > 255
      then case tailN s 2 of ",X" -> AbsoluteX
                             ",Y" -> AbsoluteY
                             _    -> Absolute
      else case tailN s 2 of ",X" -> ZeroPageX
                             ",Y" -> ZeroPageY
                             _    -> ZeroPage
  | a == '(' && last as == ')'    = Indirect
  | otherwise                     = Relative -- TODO change to better requirements for Relative parameter type
  where
    noXY n = case tailN n 2 of ",X" -> initN n (length n - 2)
                               ",Y" -> initN n (length n - 2)
                               _    -> n

getOpcodeByParamType :: String -> ParameterType -> Maybe String
getOpcodeByParamType instr pt = if instrExistsByName instr
  then case pt of
    Accumulator -> Just $ accumulator opLst
    Immediate   -> Just $ immediate opLst
    ZeroPage    -> Just $ zeroPage opLst
    ZeroPageX   -> Just $ zeroPageX opLst
    ZeroPageY   -> Just $ zeroPageY opLst
    Absolute    -> Just $ absolute opLst
    AbsoluteX   -> Just $ absoluteX opLst
    AbsoluteY   -> Just $ absoluteY opLst
    IndirectX   -> Just $ indirectX opLst
    IndirectY   -> Just $ indirectY opLst
    Relative    -> Just $ relative opLst
    Implied     -> Just $ implied opLst
    Indirect    -> Just $ indirect opLst
    Invalid     -> Nothing
    Unknown     -> Nothing
  else Nothing
  where
    opLst = opcodeList $ getInstrByName' instr

