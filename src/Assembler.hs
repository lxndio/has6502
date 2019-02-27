module Assembler
( assembleLine ) where

import Instructions (getInstrByName', instrNeedsParameter)
import Types

assembleLine :: TokenList -> Environment -> Environment
assembleLine (t:ts) env = case tokenType t of
  Label -> assembleLine ts $ Environment (prgmCtr env)
                                         (labelList env ++ [(prgmCtr env, value t)])
                                         (output env)
  Instr -> if instrNeedsParameter $ value t
    then undefined
    else assembleLine ts $ Environment (prgmCtr env + 8)
                                       (labelList env)
                                       (output env ++ (implied $ opcodeList $ getInstrByName' $ value t))
  Param -> env