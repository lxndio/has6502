module Assembler
( assembleLine ) where

import Instructions (getInstrByName', instrNeedsParameter)
import Types

assembleLine :: TokenList -> Environment -> Either ParseError Environment
assembleLine ts env = loop ts 0 env where
  loop :: TokenList -> Int -> Environment -> Either ParseError Environment
  loop ts i env
    | i < length ts = case tokenType t of
        Label -> loop ts (i+1) $ Environment (prgmCtr env)
                                             (labelList env ++ [(value t, Just $ prgmCtr env)])
                                             (output env)
        Instr -> if instrNeedsParameter $ value t
          then undefined
          else loop ts (i+1) $ Environment (prgmCtr env + 8)
                                           (labelList env)
                                           (output env ++ (implied $ opcodeList $ getInstrByName' $ value t))
        Param -> if instrNeedsParameter $ value (ts !! (i-1))
          then undefined
          else Left $ ParseError 0 $ "Parameter after implied instruction:\t'" ++ value t ++ "'"
    | otherwise     = Right env
    where
      t = ts !! i