module Types where

data ParameterType = Accumulator | Immediate | ZeroPage  | ZeroPageX | ZeroPageY | Absolute | AbsoluteX
                     | AbsoluteY | IndirectX | IndirectY | Relative  | Implied   | Indirect | Invalid deriving (Show)

data OpcodeList = OpcodeList { accumulator :: String
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

data Instruction = Instruction { name       :: String
                               , opcodeList :: OpcodeList
                               } deriving (Show)

data Line = Line { label  :: Maybe String
                 , bytes  :: Maybe String
                 } deriving (Show)

data ParseError = ParseError { line :: Int
                             , msg  :: String
                             } deriving (Show)