module Parameters
( getParameters
, validateParameters ) where

import Instructions (instrExistsByName)
import Opcodes
import Utils (isLabel)

data ParameterType = Accumulator | Immediate | ZeroPage  | ZeroPageX | ZeroPageY | Absolute | AbsoluteX
                     | AbsoluteY | IndirectX | IndirectY | Relative  | Implied   | Indirect | Invalid deriving (Show)

-- Get all parameters from a tokenized line
getParameters :: [String] -> Maybe [String]
getParameters l = if length pl > 0 then Just pl else Nothing where
  pl = dropWhile cond l
  cond x = isLabel x || instrExistsByName x

-- Validates all parameters from a tokenized line
-- TODO handle invalid parameters
validateParameters :: [String] -> Maybe [(ParameterType, String)]
validateParameters l = case getParameters l of Just ps -> Just $ validateParameters' ps
                                               Nothing -> Nothing
  where
    validateParameters' :: [String] -> [(ParameterType, String)]
    validateParameters' []     = []
    validateParameters' (l:ls) = parse l : validateParameters' ls where
      parse l = case l of "A"      -> (Accumulator, l)
                          ('#':l') -> (Immediate, l')
                          _        -> (Invalid, l)