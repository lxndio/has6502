module Parser () where

import Data.Char (isLetter, isNumber)

import Hex (isHex)
import Instructions (instrExistsByName)
import TypesNew
import Utils (parseString, initN, tailN, tokenizeString)


-- Returns the label of a line (if it has one)
-- A label always has to be the first token and has to end with a colon
getLabel :: TokenizedLine -> Maybe String
getLabel tl
  | length tl > 0 = if isLabel $ head tl then Just $ init $ head tl else Nothing
  | otherwise     = Nothing

-- Checks if a given string has the format required to be label
isLabel :: String -> Bool
isLabel = (== ':') . last

-- Checks if a given string is a valid name for a label
-- Requirements:
--   - First character is a letter
--   - Last character is a colon
--   - Consists only of letters, numbers, hyphens and underscores
isValidLabel :: String -> Bool
isValidLabel s = isLetter (head s) && last s == ':' &&
                 parseString (init s) (\c -> isLetter c || isNumber c || c == '-' || c == '_') where

-- Checks if a given string a valid numeric value
-- (decimal or hexadecimal; including immediate values using #)
isNumericVal :: String -> Bool
isNumericVal s
  | parseString s' isNumber = True
  | parseString s' isHex    = True
  | otherwise               = False
  where
    s' = case head s of '#' -> tail s
                        _   -> s

-- Checks if a given string is a valid parameter
-- Requirements:
--   - It must be
--       - a valid numeric value (decimal or hexadecimal; including immediate values using #) or
--       - a valid label (without a colon at the end)
--   - A number or a label can be followed by ,X or ,Y
isValidParam :: String -> Bool
isValidParam s
  | isNumericVal s' = True
  | isValidLabel s' = True
  | otherwise       = False
  where
    s' = case tailN s 2 of ",X" -> initN s 2
                           ",Y" -> initN s 2
                           _    -> s

detectTokenType :: String -> Either ParseError TokenType
detectTokenType t
  | isValidLabel t      = Right Label
  | instrExistsByName t = Right Instr
  | isValidParam t      = Right Param
  | otherwise           = Left $ ParseError 0 $ "Invalid token: " ++ t

-- TODO Trim and convert everything to uppercase
parseLine :: String -> Either ParseError Line
parseLine l = do
  let tl = tokenizeString l
--  tokenTypes <- detectTokenType <$> tl
  return $ Line Nothing Nothing Nothing