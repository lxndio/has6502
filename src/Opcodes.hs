module Opcodes
() where

import Instructions
import Types

getOpcodeForInstr :: String -> ParameterType -> Maybe String
getOpcodeForInstr instr pt = if opcode == "" then Nothing else Just opcode where
  opcode = case pt of Accumulator -> accumulator $ opcodeList $ getInstrByName instr
                      Immediate   -> immediate $ opcodeList $ getInstrByName instr
                      otherwise   -> ""
  instr i = case getInstrByName i of Just i'  -> i'
                                     Nothing -> undefined -- TODO replace undefined with something useful

-- unsafe
--getOpcodeForInstr' :: String -> ParameterType -> String
--getOpcodeForInstr' instr pt = pt opcodes getInstrByName

existsOpcodeForInstr :: String -> ParameterType -> Bool
existsOpcodeForInstr = undefined

--generateOpcode :: String -> [(ParameterType, String)] -> Either ParseError String
--generateOpcode instr params = parse params where
--  parse (pt, v) = if existsOpcodeForInstr instr pt then Right $ getOpcodeForInstr' else Left $ ParseError 0 "blub"