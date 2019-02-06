module Instructions
( Instruction
, getInstrByName
, instrExistsByName
, getInstruction
, getInstruction' ) where

import Utils

data Opcodes = Opcodes { accumulator :: String
                       , immediate   :: String
                       , zeroPage    :: String
                       , zeroPageX   :: String
                       , zeroPageY   :: String
                       , absolute    :: String
                       , absoluteX   :: String
                       , absoluteY   :: String
                       , indirectX   :: String
                       , indirectY   :: String
                       , relative    :: String
                       , implied     :: String
                       , indirect    :: String
                       } deriving (Show)

data Instruction = Instruction { name    :: String
                               , opcodes :: Opcodes
                               } deriving (Show)

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

-- Get the first occurence of an instruction in a tokenized line
-- Returns the empty instruction if the line does not contain an instruction
getInstruction' :: [String] -> Maybe Instruction
getInstruction' l
  | isLineEmpty l = getInstrByName "emp"
  | otherwise     = getInstruction l

instructions :: [Instruction]
--               Instruction Name           acc  imm  zPa  zPaX zPaY abs  absX absY indX indY rel  impl ind
instructions = [ Instruction "ADC" (Opcodes ""   "69" "65" "75" ""   "6D" "7D" "79" "61" "71" ""   ""   ""  )
               , Instruction "AND" (Opcodes ""   "29" "25" "35" ""   "2D" "3D" "39" "21" "31" ""   ""   ""  )
               , Instruction "ASL" (Opcodes "0A" ""   "06" "16" ""   "0E" "1E" ""   ""   ""   ""   ""   ""  )
               , Instruction "BCC" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "90" ""   ""  )
               , Instruction "BCS" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "B0" ""   ""  )
               , Instruction "BEQ" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "F0" ""   ""  )
               , Instruction "BIT" (Opcodes ""   ""   "24" ""   ""   "2C" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "BMI" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "30" ""   ""  )
               , Instruction "BNE" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "D0" ""   ""  )
               , Instruction "BPL" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "10" ""   ""  )
               , Instruction "BRK" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "00" ""  )
               , Instruction "BVC" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "50" ""   ""  )
               , Instruction "BVS" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "70" ""   ""  )
               , Instruction "CLC" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "18" ""  )
               , Instruction "CLD" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "D8" ""  )
               , Instruction "CLI" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "58" ""  )
               , Instruction "CLV" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "B8" ""  )
               , Instruction "CMP" (Opcodes ""   "C9" "C5" "D5" ""   "CD" "DD" "D9" "C1" "D1" ""   ""   ""  )
               , Instruction "CPX" (Opcodes ""   "E0" "E4" ""   ""   "EC" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "CPY" (Opcodes ""   "C0" "C4" ""   ""   "CC" ""   ""   ""   ""   ""   ""   ""  )
--               Instruction Name           acc  imm  zPa  zPaX zPaY abs  absX absY indX indY rel  impl ind
               , Instruction "DEC" (Opcodes ""   ""   "C6" "D6" ""   "CE" "DE" ""   ""   ""   ""   ""   ""  )
               , Instruction "DEX" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "CA" ""  )
               , Instruction "DEY" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "88" ""  )
               , Instruction "EOR" (Opcodes ""   "49" "45" "55" ""   "4D" "5D" "59" "41" "51" ""   ""   ""  )
               , Instruction "INC" (Opcodes ""   ""   "E6" "F6" ""   "EE" "FE" ""   ""   ""   ""   ""   ""  )
               , Instruction "INX" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "E8" ""  )
               , Instruction "INY" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "C8" ""  )
               , Instruction "JMP" (Opcodes ""   ""   ""   ""   ""   "4C" ""   ""   ""   ""   ""   ""   "6C")
               , Instruction "JSR" (Opcodes ""   ""   ""   ""   ""   "20" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "LDA" (Opcodes ""   "A9" "A5" "B5" ""   "AD" "BD" "B9" "A1" "B1" ""   ""   ""  )
               , Instruction "LDX" (Opcodes ""   "A2" "A6" ""   "B6" "AE" ""   "BE" ""   ""   ""   ""   ""  )
               , Instruction "LDY" (Opcodes ""   "A0" "A4" "B4" ""   "AC" "BC" ""   ""   ""   ""   ""   ""  )
               , Instruction "LSR" (Opcodes "4A" ""   "46" "56" ""   "4E" "5E" ""   ""   ""   ""   ""   ""  )
               , Instruction "NOP" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "EA" ""  )
               , Instruction "ORA" (Opcodes ""   "09" "05" "15" ""   "0D" "1D" "19" "01" "11" ""   ""   ""  )
               , Instruction "PHA" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "48" ""  )
               , Instruction "PHP" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "08" ""  )
               , Instruction "PLA" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "68" ""  )
               , Instruction "PLP" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "28" ""  )
               , Instruction "ROL" (Opcodes "2A" ""   "26" "36" ""   "2E" "3E" ""   ""   ""   ""   ""   ""  )
               , Instruction "ROR" (Opcodes "6A" ""   "66" "76" ""   "6E" "7E" ""   ""   ""   ""   ""   ""  )
--               Instruction Name           acc  imm  zPa  zPaX zPaY abs  absX absY indX indY rel  impl ind
               , Instruction "RTI" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "40" ""  )
               , Instruction "RTS" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "60" ""  )
               , Instruction "SBC" (Opcodes ""   "E9" "E5" "F5" ""   "ED" "FD" "F9" "E1" "F1" ""   ""   ""  )
               , Instruction "SEC" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "38" ""  )
               , Instruction "SED" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "F8" ""  )
               , Instruction "SEI" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "78" ""  )
               , Instruction "STA" (Opcodes ""   ""   "85" "95" ""   "8D" "9D" "99" "81" "91" ""   ""   ""  )
               , Instruction "STX" (Opcodes ""   ""   "86" ""   "96" "8E" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "STY" (Opcodes ""   ""   "84" "94" ""   "8C" ""   ""   ""   ""   ""   ""   ""  )
               , Instruction "TAX" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "AA" ""  )
               , Instruction "TAY" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "A8" ""  )
               , Instruction "TSX" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "BA" ""  )
               , Instruction "TXA" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "8A" ""  )
               , Instruction "TXS" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "9A" ""  )
               , Instruction "TYA" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   "98" ""  )
               , Instruction "emp" (Opcodes ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""   ""  ) -- empty line
               ]