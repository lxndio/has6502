module TypesNew where

data Line = Line { label :: Maybe String
                 , instr :: Maybe String
                 , param :: Maybe String }

data ParseError = ParseError { line :: Int
                             , text :: String } deriving (Show)

type TokenizedLine = [String]

data TokenType = Label | Instr | Param deriving (Show)