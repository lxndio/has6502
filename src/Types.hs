module Types where

data ParseError = ParseError { line :: Int
                             , text :: String } deriving (Show)

type ParseErrors = [ParseError]

type TokenizedString = [String]

data TokenType = Label | Instr | Param deriving (Show)

instance Eq TokenType where
  Label == Label = True
  Instr == Instr = True
  Param == Param = True
  _     == _     = False

data Token = Token { tokenType :: TokenType
                   , value     :: String    } deriving (Show)

type TokenList = [Token]

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

data Environment = Environment { prgmCtr   :: Int
                               , labelList :: [(Int, String)]
                               , output    :: String
                               } deriving (Show)