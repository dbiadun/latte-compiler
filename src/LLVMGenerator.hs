module LLVMGenerator where

import AbsLatte
import Control.Monad.Except
import qualified Data.Map as Map
import ErrM
import GeneratorMonad
import LLVMInstructions
import Types

-------------------------------------------------------------------------------

failure :: Show a => a -> Result
failure x = throwError $ VariableNotDefined $ Ident $ show x

-------------------------------------------------------------------------------

--genIdent :: Ident -> Result
--genIdent x = case x of
--  Ident string -> failure x

-- Program --------------------------------------------------------------------

genLLVM :: Show a => Program a -> String
genLLVM p = case runGenM initialState $ genProgram p of
  Left err -> show err
  Right (state, instructions) -> unlines $ map show $ addFirstLines state instructions

genProgram :: Show a => Program a -> GenM ()
genProgram x = case x of
  Program _ topdefs -> mapM_ declareFunction topdefs >> mapM_ genTopDef topdefs

addFirstLines :: GenState -> [Instruction] -> [Instruction]
addFirstLines s instructions = genLiteralsDefinitions s ++ instructions

-------------------------------------------------

genLiteralsDefinitions :: GenState -> [Instruction]
genLiteralsDefinitions s = map DefineStrLiteral $ runOnSLenvMap Map.elems $ slenv s

-- Functions ------------------------------------------------------------------

declareFunction :: Show a => TopDef a -> GenM ()
declareFunction (FnDef _ type_ id args block) = do
  let returnType = genType type_
  let argTypes = map getArgType args
  addFunction id returnType

genTopDef :: Show a => TopDef a -> GenM ()
genTopDef x = case x of
  FnDef _ type_ ident args block -> do
    runIsolated $ do
      loadedArgs <- mapM genArg args
      let retType = genType type_
      emit $ FnStart $ FuncHeader retType ident loadedArgs
      genBlock block
      emit FnEnd

genArg :: Show a => Arg a -> GenM FuncArg
genArg x = case x of
  Arg _ type_ ident -> do
    let t = genType type_
    var <- addVarVal t ident
    return $ FuncArg var

genBlock :: Show a => Block a -> GenM ()
genBlock x = case x of
  Block _ stmts -> mapM_ genStmt stmts

-------------------------------------------------

getArgType :: Show a => Arg a -> ValueType
getArgType (Arg _ t _) = genType t

-- Statements -----------------------------------------------------------------

genStmt :: Show a => Stmt a -> GenM ()
genStmt x = case x of
  Empty _ -> return ()
  BStmt _ block -> runIsolated $ genBlock block
  Decl _ type_ items -> do
    let t = genType type_
    mapM_ (genItem t) items
  Ass _ ident expr -> assign ident expr
  --  Incr _ ident -> failure x
  --  Decr _ ident -> failure x
  Ret _ expr -> do
    var <- genExpr expr
    emit $ RetInst var
  VRet _ -> emit VRetInst
  --  Cond _ expr stmt -> failure x
  --  CondElse _ expr stmt1 stmt2 -> failure x
  --  While _ expr stmt -> failure x
  SExp _ expr -> void $ genExpr expr

genItem :: Show a => ValueType -> Item a -> GenM ()
genItem t x = case x of
  NoInit _ ident -> initialize t ident
  Init _ ident expr -> initialize t ident >> assign ident expr

genType :: Show a => Type a -> ValueType
genType x = case x of
  Int _ -> IntT
  Str _ -> StrT
  Bool _ -> BoolT
  Void _ -> VoidT
  Fun _ type_ types -> NoneT

-------------------------------------------------

initialize :: ValueType -> Ident -> GenM ()
initialize t id = do
  var <- addVarAddr t id
  emit $ AllocaInst var

--  case varType var of
--    StrT -> do
--      let sizeVar_ = sizeVar var
--      emit $ AllocaInst sizeVar_
--    _ -> return ()

assign :: Show a => Ident -> Expr a -> GenM ()
assign id expr = do
  var <- getVar id
  exprVar <- genExpr expr
  emit $ StoreInst exprVar var

--  case varType exprVar of
--    StrT -> do
--      let sizeVar_ = sizeVar var
--      let exprSizeVar = sizeVar exprVar
--      emit $ StoreInst exprSizeVar sizeVar_
--    _ -> return ()

-------------------------------------------------

--createString :: Ident -> Var -> GenM ()
--createString id tempSize = do
--  str <- getVar id
--  let sizeVar_ = sizeVar str
--  tempStr <- getTempVarVal StrT
--  emit $ MallocStr tempStr tempSize
--  emit $ StoreInst tempStr str
--  emit $ StoreInst tempSize sizeVar_

-- Expressions ----------------------------------------------------------------

genExpr :: Show a => Expr a -> GenM Var
genExpr x = case x of
  EVar _ ident -> do
    var <- getVar ident
    case var of
      VarAddr t _ -> do
        tempVar <- getTempVarVal t
        emit $ LoadInst tempVar var
        return tempVar
      _ -> return var
  ELitInt _ integer -> return $ VarConst IntT $ IntV integer
  ELitTrue _ -> return $ VarConst BoolT $ BoolV True
  ELitFalse _ -> return $ VarConst BoolT $ BoolV False
  EApp _ ident exprs -> do
    t <- getFunctionType ident
    temp <- getTempVarVal t
    args <- mapM genExpr exprs
    emit $ CallInst temp ident args
    return temp
  EString _ string -> do
    let str = read string
    sl <- addStrLiteral str
    var <- getTempVarVal StrT
    emit $ AssignStrLiteral var sl
    return var

--  Neg _ expr -> failure x
--  Not _ expr -> failure x
--  EMul _ expr1 mulop expr2 -> failure x
--  EAdd _ expr1 addop expr2 -> failure x
--  ERel _ expr1 relop expr2 -> failure x
--  EAnd _ expr1 expr2 -> failure x
--  EOr _ expr1 expr2 -> failure x

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
