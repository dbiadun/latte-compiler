module LLVMInstructions where

import Types

-- Types ----------------------------------------------------------------------

data Instruction
  = AddInst Register Var Var
  | SubInst Register Var Var
  | MulInst Register Var Var
  deriving (Show)

-- Operations -----------------------------------------------------------------
