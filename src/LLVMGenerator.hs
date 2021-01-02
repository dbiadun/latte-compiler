module LLVMGenerator where

import AbsLatte
import Control.Monad.Except
import ErrM
import GeneratorMonad

-------------------------------------------------------------------------------

failure :: Show a => a -> Result
failure x = throwError $ VariableNotDefined $ Ident $ show x

-------------------------------------------------------------------------------

--genIdent :: Ident -> Result
--genIdent x = case x of
--  Ident string -> failure x

-------------------------------------------------------------------------------

genLLVM :: Show a => Program a -> [String]
genLLVM p = case runGenM initialState $ genProgram p of
  Left err -> [show err]
  Right (state, instructions) -> map show instructions

genProgram :: Show a => Program a -> GenM ()
genProgram x = case x of
  Program _ topdefs -> mapM_ genTopDef topdefs

genTopDef :: Show a => TopDef a -> GenM ()
genTopDef x = case x of
  FnDef _ type_ ident args block -> failure x

genArg :: Show a => Arg a -> Result
genArg x = case x of
  Arg _ type_ ident -> failure x

genBlock :: Show a => Block a -> Result
genBlock x = case x of
  Block _ stmts -> failure x

genStmt :: Show a => Stmt a -> Result
genStmt x = case x of
  Empty _ -> failure x
  BStmt _ block -> failure x
  Decl _ type_ items -> failure x
  Ass _ ident expr -> failure x
  Incr _ ident -> failure x
  Decr _ ident -> failure x
  Ret _ expr -> failure x
  VRet _ -> failure x
  Cond _ expr stmt -> failure x
  CondElse _ expr stmt1 stmt2 -> failure x
  While _ expr stmt -> failure x
  SExp _ expr -> failure x

genItem :: Show a => Item a -> Result
genItem x = case x of
  NoInit _ ident -> failure x
  Init _ ident expr -> failure x

genType :: Show a => Type a -> Result
genType x = case x of
  Int _ -> failure x
  Str _ -> failure x
  Bool _ -> failure x
  Void _ -> failure x
  Fun _ type_ types -> failure x

genExpr :: Show a => Expr a -> Result
genExpr x = case x of
  EVar _ ident -> failure x
  ELitInt _ integer -> failure x
  ELitTrue _ -> failure x
  ELitFalse _ -> failure x
  EApp _ ident exprs -> failure x
  EString _ string -> failure x
  Neg _ expr -> failure x
  Not _ expr -> failure x
  EMul _ expr1 mulop expr2 -> failure x
  EAdd _ expr1 addop expr2 -> failure x
  ERel _ expr1 relop expr2 -> failure x
  EAnd _ expr1 expr2 -> failure x
  EOr _ expr1 expr2 -> failure x

genAddOp :: Show a => AddOp a -> Result
genAddOp x = case x of
  Plus _ -> failure x
  Minus _ -> failure x

genMulOp :: Show a => MulOp a -> Result
genMulOp x = case x of
  Times _ -> failure x
  Div _ -> failure x
  Mod _ -> failure x

genRelOp :: Show a => RelOp a -> Result
genRelOp x = case x of
  LTH _ -> failure x
  LE _ -> failure x
  GTH _ -> failure x
  GE _ -> failure x
  EQU _ -> failure x
  NE _ -> failure x
