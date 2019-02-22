module Parser
( parseLine
, parseLines ) where

import Data.Char (isLetter, isNumber)

import Hex (isHex)
import Instructions (getInstrByName', instrExistsByName, instrNeedsParameter)
import Types
import Utils (parseString, initN, tailN, tokenizeString, addLineNumbers)

-- Returns the label of a line (if it has one)
-- A label always has to be the first token and has to end with a colon
getLabel :: TokenizedString -> Maybe String
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

-- Same as isValidLabel but requires that there is no colon at the end of the label's name
isValidLabelAsParam :: String -> Bool
isValidLabelAsParam s = parseString s (\c -> isLetter c || isNumber c || c == '-' || c == '_') where

-- Checks if a given string a valid numeric value
-- (decimal or hexadecimal; including immediate values using #)
isNumericVal :: String -> Bool
isNumericVal (a:as)
  | parseString as isNumber          = True
  | a == '$' && parseString as isHex = True
  | a == '#'                         = isNumericVal as
  | otherwise                        = False

-- Checks if a given string is a valid parameter
-- Requirements:
--   - It must be
--       - a valid numeric value (decimal or hexadecimal; including immediate values using #) or
--       - a valid label (without a colon at the end)
--   - A number or a label can be followed by ,X or ,Y
isValidParam :: String -> Bool
isValidParam s
  | isNumericVal s'        = True
  | isValidLabelAsParam s' = True
  | otherwise              = False
  where
    s' = case tailN s 2 of ",X" -> initN s (length s - 2)
                           ",Y" -> initN s (length s - 2)
                           _    -> s

getTokenType :: String -> Either ParseError TokenType
getTokenType t
  | isValidLabel t      = Right Label
  | instrExistsByName t = Right Instr
  | isValidParam t      = Right Param
  | otherwise           = Left $ ParseError 0 $ "Invalid token: " ++ t

getParseErrors :: TokenizedString -> Maybe ParseErrors
getParseErrors ts = if length (loop ts) == 0 then Nothing else Just $ loop ts where
  loop :: TokenizedString -> ParseErrors
  loop (t:ts) = case getTokenType t of
    Left pe -> pe : loop ts
    Right _ -> loop ts
  loop []     = []

parseTokens :: TokenizedString -> Either ParseErrors TokenList
parseTokens ts = case getParseErrors ts of
  Just pes -> Left pes
  Nothing  -> Right $ loop ts
    where
      loop :: TokenizedString -> TokenList
      loop (t:ts) = case getTokenType t of
        Right tt -> Token tt t : loop ts
        Left  _  -> loop ts -- Should not happen
      loop []     = []

evalTokenOrder :: TokenList -> Either ParseErrors Bool
evalTokenOrder tl = if length (loop tl 0) == 0 then Right True else Left $ loop tl 0 where
  loop :: TokenList -> Int -> ParseErrors
  loop (t:ts) cnt = case tokenType t of
    Label -> if cnt == 0 then loop ts (cnt+1) else (ParseError 0 ("Label at wrong position:\t'" ++ value t ++ "'")) : loop ts (cnt+1)
    Instr -> if cnt == 0 || cnt == 1 then loop ts (cnt+1) else (ParseError 0 ("Instruction at wrong position:\t'" ++ value t ++ "'")) : loop ts (cnt+1)
    Param -> if cnt /= 0 then loop ts (cnt+1) else (ParseError 0 ("Parameter at wrong position:\t'" ++ value t ++ "'")) : loop ts (cnt+1)
  loop []     _   = []

-- Checks if there is an instruction before each parameter
-- and a parameter after each instruction
evalParameterExistence :: TokenList -> Either ParseErrors Bool
evalParameterExistence tl = if length (loop tl 0) == 0 then Right True else Left $ loop tl 0 where
  loop :: TokenList -> Int -> ParseErrors
  loop tl cnt
    | cnt < length tl = let current = tl !! cnt in
      case tokenType current of
        Label -> loop tl (cnt+1)
        Instr -> if not $ instrNeedsParameter $ value current
          then loop tl (cnt+1)
          else if cnt < (length tl - 1) && tokenType (tl !! (cnt+1)) == Param
            then loop tl (cnt+1)
            else (ParseError 0 ("Instruction without parameter:\t'" ++ value current ++ "'")) : loop tl (cnt+1)
        Param -> if tokenType (tl !! (cnt-1)) == Instr
          then loop tl (cnt+1)
          else (ParseError 0 ("Parameter without instruction:\t'" ++ value current ++ "'")) : loop tl (cnt+1)
    | cnt >= length tl = []

parseLine :: String -> Either ParseErrors TokenList
parseLine l = do
  -- Tokenize the given String and remove comments (everything after a semicolon and the semicolon itself)
  let tl = tokenizeString $ takeWhile (/= ';') l
  tokens <- parseTokens tl
  evalTokenOrder tokens
  evalParameterExistence tokens
  return tokens

parseLines :: [String] -> Either ParseErrors TokenList
parseLines ls = let res = loop ls 1 in
  if length (fst res) == 0 then Right $ snd res else Left $ fst res where
    loop :: [String] -> Int -> (ParseErrors, TokenList)
    loop (l:ls) lineNr = let next = loop ls (lineNr+1) in
      case parseLine l of
        Left pes -> (addLineNumbers pes lineNr ++ fst next, snd next)
        Right tl -> (fst next, snd next ++ tl)
    loop []     _ = ([], [])