module Opcodes
( generateOpcodes ) where

import Instruction (Instruction)
import Parameters (ParameterType)
import Parser (ParseError)

getOpcodeForInstr :: String -> ParameterType -> Maybe String
getOpcodeForInstr instr pt = if opcode == "" then Nothing else Just opcode where
  opcode = pt opcodes getInstrByName

-- unsafe
getOpcodeForInstr' :: String -> ParameterType -> String
getOpcodeForInstr' instr pt = pt opcodes getInstrByName

existsOpcodeForInstr :: String -> ParameterType -> Bool
existsOpcodeForInstr = undefined

generateOpcodes :: String -> [(ParameterType, String)] -> Either ParseError String
generateOpcodes instr params = parse params where
  parse (pt, v) = if existsOpcodeForInstr instr pt then Right $ getOpcodeForInstr' else Left $ ParseError 0 "blub"