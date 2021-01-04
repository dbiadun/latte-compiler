module LLVMInstructions where

import Types

-- Types ----------------------------------------------------------------------

data Instruction
  = FnStart FuncHeader
  | FnEnd
  | RetInst Var
  | VRetInst

-- Show -----------------------------------------------------------------------

instance Show Instruction where
  show (FnStart header) = show header
  show FnEnd = "}"
  show (RetInst var) = "ret " ++ show var
  show VRetInst = "ret void"
