module Instructions
( getInstrByName
, getInstrByName'
, instrExistsByName
, getInstruction
, getInstruction' ) where

import Types
import Utils

-- TODO delete
isLineEmpty = undefined

getInstrByName :: String -> Maybe Instruction
getInstrByName s = if length instructions' == 0 then Nothing else Just $ head $ instructions' where
  instructions' = filter ((== s) . name) instructions

-- Unsafe version of getInstrByName
getInstrByName' :: String -> Instruction
getInstrByName' s = head $ filter ((== s) . name) instructions

instrExistsByName :: String -> Bool
instrExistsByName s = if length instructions' == 0 then False else True where
  instructions' = filter ((== s) . name) instructions

-- Get the first occurrence of an instruction in a tokenized line
getInstruction :: [String] -> Maybe Instruction
getInstruction (l:ls) = case getInstrByName l of i@(Just _) -> i
                                                 Nothing    -> getInstruction ls
getInstruction []     = Nothing

-- Get the first occurence of an instruction in a tokenized line
-- Returns the empty instruction if the line does not contain an instruction
getInstruction' :: [String] -> Maybe Instruction
getInstruction' l
  | isLineEmpty l = getInstrByName "emp"
  | otherwise     = getInstruction l

instructions :: [Instruction]
--               Instruction Name              acc  imm  zPa  zPaX zPaY abs  absX absY indX indY rel  impl ind
instructions = [ Instruction "ADC" (OpcodeList ""   "69" "65" "75" ""   "6D" "7D" "79" "61" "71" ""   ""   ""  )
               , Instruction "AND" (OpcodeList ""   "29" "25" "35" ""   "2D" "3D" "39" "21" "31" ""   ""   ""  )
               , Instruction "ASL" (OpcodeList "0A" ""   "06" "16" ""   "0E" "1E" ""   ""   ""   ""   ""   ""  )
               , Instruction "BCC" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "90" ""   ""  )
               , Instruction "BCS" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "B0" ""   ""  )
               , Instruction "BEQ" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "F0" ""   ""  )
               , Instruction "BIT" (OpcodeList ""   ""   "24" ""   ""   "2C" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "BMI" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "30" ""   ""  )
               , Instruction "BNE" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "D0" ""   ""  )
               , Instruction "BPL" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "10" ""   ""  )
               , Instruction "BRK" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "00" ""  )
               , Instruction "BVC" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "50" ""   ""  )
               , Instruction "BVS" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "70" ""   ""  )
               , Instruction "CLC" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "18" ""  )
               , Instruction "CLD" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "D8" ""  )
               , Instruction "CLI" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "58" ""  )
               , Instruction "CLV" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "B8" ""  )
               , Instruction "CMP" (OpcodeList ""   "C9" "C5" "D5" ""   "CD" "DD" "D9" "C1" "D1" ""   ""   ""  )
               , Instruction "CPX" (OpcodeList ""   "E0" "E4" ""   ""   "EC" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "CPY" (OpcodeList ""   "C0" "C4" ""   ""   "CC" ""   ""   ""   ""   ""   ""   ""  )
--               Instruction Name              acc  imm  zPa  zPaX zPaY abs  absX absY indX indY rel  impl ind
               , Instruction "DEC" (OpcodeList ""   ""   "C6" "D6" ""   "CE" "DE" ""   ""   ""   ""   ""   ""  )
               , Instruction "DEX" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "CA" ""  )
               , Instruction "DEY" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "88" ""  )
               , Instruction "EOR" (OpcodeList ""   "49" "45" "55" ""   "4D" "5D" "59" "41" "51" ""   ""   ""  )
               , Instruction "INC" (OpcodeList ""   ""   "E6" "F6" ""   "EE" "FE" ""   ""   ""   ""   ""   ""  )
               , Instruction "INX" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "E8" ""  )
               , Instruction "INY" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "C8" ""  )
               , Instruction "JMP" (OpcodeList ""   ""   ""   ""   ""   "4C" ""   ""   ""   ""   ""   ""   "6C")
               , Instruction "JSR" (OpcodeList ""   ""   ""   ""   ""   "20" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "LDA" (OpcodeList ""   "A9" "A5" "B5" ""   "AD" "BD" "B9" "A1" "B1" ""   ""   ""  )
               , Instruction "LDX" (OpcodeList ""   "A2" "A6" ""   "B6" "AE" ""   "BE" ""   ""   ""   ""   ""  )
               , Instruction "LDY" (OpcodeList ""   "A0" "A4" "B4" ""   "AC" "BC" ""   ""   ""   ""   ""   ""  )
               , Instruction "LSR" (OpcodeList "4A" ""   "46" "56" ""   "4E" "5E" ""   ""   ""   ""   ""   ""  )
               , Instruction "NOP" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "EA" ""  )
               , Instruction "ORA" (OpcodeList ""   "09" "05" "15" ""   "0D" "1D" "19" "01" "11" ""   ""   ""  )
               , Instruction "PHA" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "48" ""  )
               , Instruction "PHP" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "08" ""  )
               , Instruction "PLA" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "68" ""  )
               , Instruction "PLP" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "28" ""  )
               , Instruction "ROL" (OpcodeList "2A" ""   "26" "36" ""   "2E" "3E" ""   ""   ""   ""   ""   ""  )
               , Instruction "ROR" (OpcodeList "6A" ""   "66" "76" ""   "6E" "7E" ""   ""   ""   ""   ""   ""  )
--               Instruction Name              acc  imm  zPa  zPaX zPaY abs  absX absY indX indY rel  impl ind
               , Instruction "RTI" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "40" ""  )
               , Instruction "RTS" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "60" ""  )
               , Instruction "SBC" (OpcodeList ""   "E9" "E5" "F5" ""   "ED" "FD" "F9" "E1" "F1" ""   ""   ""  )
               , Instruction "SEC" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "38" ""  )
               , Instruction "SED" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "F8" ""  )
               , Instruction "SEI" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "78" ""  )
               , Instruction "STA" (OpcodeList ""   ""   "85" "95" ""   "8D" "9D" "99" "81" "91" ""   ""   ""  )
               , Instruction "STX" (OpcodeList ""   ""   "86" ""   "96" "8E" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "STY" (OpcodeList ""   ""   "84" "94" ""   "8C" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "TAX" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "AA" ""  )
               , Instruction "TAY" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "A8" ""  )
               , Instruction "TSX" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "BA" ""  )
               , Instruction "TXA" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "8A" ""  )
               , Instruction "TXS" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "9A" ""  )
               , Instruction "TYA" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "98" ""  )
               , Instruction "emp" (OpcodeList ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""  ) -- empty line
               ]