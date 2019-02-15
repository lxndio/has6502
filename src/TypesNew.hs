module TypesNew where

data Line = Line { label :: Maybe String
                 , instr :: Maybe String
                 , param :: Maybe String } deriving (Show)

data ParseError = ParseError { line :: Int
                             , text :: String } deriving (Show)

type ParseErrors = [ParseError]

type TokenizedString = [String]

data TokenType = Label | Instr | Param deriving (Show)

data Token = Token { tokenType :: TokenType
                   , value     :: String    } deriving (Show)

type TokenList = [Token]