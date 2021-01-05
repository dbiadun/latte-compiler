module LLVMInstructions where

import AbsLatte
import Data.List (intercalate)
import Types

-- Types ----------------------------------------------------------------------

data Instruction
  = FnStart FuncHeader
  | FnEnd
  | RetInst Var
  | VRetInst
  | AllocaInst Var
  | StoreInst Var Var
  | LoadInst Var Var
  | MallocStr Var Var
  | CallInst Var Ident [Var]

-- Show -----------------------------------------------------------------------

instance Show Instruction where
  show (FnStart header) = show header
  show FnEnd = "}"
  show (RetInst var) = "ret " ++ show var
  show VRetInst = "ret void"
  show (AllocaInst var) = showSimple var ++ " = alloca " ++ show (varType var)
  show (StoreInst val container) = "store " ++ show val ++ ", " ++ show container
  show (LoadInst var container) = showSimple var ++ " = load " ++ show (varType var) ++ ", " ++ show container
  show (MallocStr var size) = showSimple var ++ " = call i8* @malloc(" ++ show size ++ ")"
  show (CallInst var id args) =
    let start = case varType var of
          VoidT -> ""
          _ -> showSimple var ++ " = "
     in start ++ "call " ++ show (varType var) ++ " @" ++ showId id ++ "("
          ++ intercalate ", " (map show args)
          ++ ")"
