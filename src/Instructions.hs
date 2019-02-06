module Instructions
( Instruction
, getInstrByName
, instrExistsByName
, getInstruction ) where

data Instruction = Instruction { name :: String
                               , opcode :: String
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

instructions :: [Instruction]
instructions = [ Instruction "ADC" ""
               , Instruction "AND" ""
               , Instruction "ASL" ""
               , Instruction "BCC" ""
               , Instruction "BCS" ""
               , Instruction "BEQ" ""
               , Instruction "BIT" ""
               , Instruction "BMI" ""
               , Instruction "BNE" ""
               , Instruction "BPL" ""
               , Instruction "BRK" ""
               , Instruction "BVC" ""
               , Instruction "BVS" ""
               , Instruction "CLC" ""
               , Instruction "CLD" ""
               , Instruction "CLI" ""
               , Instruction "CLV" ""
               , Instruction "CMP" ""
               , Instruction "CPX" ""
               , Instruction "CPY" ""
               , Instruction "DEC" ""
               , Instruction "DEX" ""
               , Instruction "DEY" ""
               , Instruction "EOR" ""
               , Instruction "INC" ""
               , Instruction "INX" ""
               , Instruction "INY" ""
               , Instruction "JMP" ""
               , Instruction "JSR" ""
               , Instruction "LDA" ""
               , Instruction "LDX" ""
               , Instruction "LDY" ""
               , Instruction "LSR" ""
               , Instruction "NOP" ""
               , Instruction "ORA" ""
               , Instruction "PHA" ""
               , Instruction "PHP" ""
               , Instruction "PLA" ""
               , Instruction "PLP" ""
               , Instruction "ROL" ""
               , Instruction "ROR" ""
               , Instruction "RTI" ""
               , Instruction "RTS" ""
               , Instruction "SBC" ""
               , Instruction "SEC" ""
               , Instruction "SED" ""
               , Instruction "SEI" ""
               , Instruction "STA" ""
               , Instruction "STX" ""
               , Instruction "STY" ""
               , Instruction "TAX" ""
               , Instruction "TAY" ""
               , Instruction "TSX" ""
               , Instruction "TXA" ""
               , Instruction "TXS" ""
               , Instruction "TYA" ""
               ]