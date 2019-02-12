module TypesNew where

data Line = Line { label :: Maybe String
                 , instr :: Maybe String
                 , param :: Maybe String }

data ParseError = ParseError { line :: Int
                             , text :: String }

type TokenizedLine = [String]

data TokenType = Label | Instr | Param