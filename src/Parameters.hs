module Parameters
( getParameters
, validateParameters ) where

import Instructions (instrExistsByName)
import Types
import Utils (isLabel)

-- Get all parameters from a tokenized line
getParameters :: [String] -> Maybe [String]
getParameters l = if length pl > 0 then Just pl else Nothing where
  pl = dropWhile cond l
  cond x = isLabel x || instrExistsByName x

getParameterType :: String -> ParameterType
getParameterType s = undefined