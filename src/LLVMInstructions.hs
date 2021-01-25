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
  | AssignStrLiteral Var StrLiteral
  | DefineStrLiteral StrLiteral
  | DeclareInst String
  | AddInst Var Var Var
  | SubInst Var Var Var
  | MulInst Var Var Var
  | DivInst Var Var Var
  | ModInst Var Var Var
  | LabelInst Label
  | JumpInst Label
  | CondJumpInst Var Label Label
  | ICMPInst Var String Var Var
  | UnreachableInst
  | PhiInst Var [(Var, Label)]

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
  show (AssignStrLiteral var sl) = showSimple var ++ " = getelementptr " ++ showLiteralType sl ++ ", " ++ show sl ++ ", i32 0, i32 0"
  show (DefineStrLiteral sl@(StrLiteral id size str)) =
    showSimple sl ++ " = private constant ["
      ++ show size
      ++ " x i8] c\""
      ++ str
      ++ "\""
  show (DeclareInst s) = "declare " ++ s
  show (AddInst res v1 v2) = showSimple res ++ " = add " ++ show v1 ++ ", " ++ showSimple v2
  show (SubInst res v1 v2) = showSimple res ++ " = sub " ++ show v1 ++ ", " ++ showSimple v2
  show (MulInst res v1 v2) = showSimple res ++ " = mul " ++ show v1 ++ ", " ++ showSimple v2
  show (DivInst res v1 v2) = showSimple res ++ " = sdiv " ++ show v1 ++ ", " ++ showSimple v2
  show (ModInst res v1 v2) = showSimple res ++ " = srem " ++ show v1 ++ ", " ++ showSimple v2
  show (LabelInst l) = showSimple l
  show (JumpInst l) = "br " ++ show l
  show (CondJumpInst cond l1 l2) = "br " ++ show cond ++ ", " ++ show l1 ++ ", " ++ show l2
  show (ICMPInst res cond v1 v2) = showSimple res ++ " = icmp " ++ cond ++ " " ++ show v1 ++ ", " ++ showSimple v2
  show UnreachableInst = "unreachable"
  show (PhiInst res srcs) =
    showSimple res ++ " = phi " ++ show (varType res) ++ " "
      ++ intercalate ", " (map (\(v, l) -> "[" ++ showSimple v ++ ", " ++ showLabelAlone l ++ "]") srcs)
