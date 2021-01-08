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

-- Program --------------------------------------------------------------------

genLLVM :: Show a => Program a -> String
genLLVM p = case runGenM initialState $ genProgram p of
  Left err -> show err
  Right (state, instructions, _) -> unlines $ map show $ addFirstLines state instructions

genProgram :: Show a => Program a -> GenM ()
genProgram x = case x of
  Program _ topdefs -> do
    addPredefinedFunctions
    mapM_ declareFunction topdefs
    mapM_ genTopDef topdefs

-- Top instructions -----------------------------------------------------------

addFirstLines :: GenState -> [Instruction] -> [Instruction]
addFirstLines s instructions = genDeclarations ++ genLiteralsDefinitions s ++ instructions

-------------------------------------------------

genDeclarations :: [Instruction]
genDeclarations =
  map
    DeclareInst
    [ "void @printInt(i32)",
      "void @printString(i8*)",
      "void @error()",
      "i32 @readInt()",
      "i8* @readString()",
      "i8* @concat(i8*, i8*)"
    ]

genLiteralsDefinitions :: GenState -> [Instruction]
genLiteralsDefinitions s = map DefineStrLiteral $ runOnSLenvMap Map.elems $ slenv s

-------------------------------------------------

addPredefinedFunctions :: GenM ()
addPredefinedFunctions = do
  addFunction (Ident "printInt") VoidT
  addFunction (Ident "printString") VoidT
  addFunction (Ident "error") VoidT
  addFunction (Ident "readInt") IntT
  addFunction (Ident "readString") StrT
  addFunction (Ident "concat") StrT

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
      let blockInsts = genBlock block
      modifyInstructions addNeededTerminators blockInsts
      emit FnEnd

genArg :: Show a => Arg a -> GenM FuncArg
genArg x = case x of
  Arg _ type_ ident -> do
    let t = genType type_
    var <- addVarVal t ident
    return $ FuncArg var

genBlock :: Show a => Block a -> GenM Bool
genBlock x = case x of
  Block pos stmts ->
    case stmts of
      [] -> return False
      [s] -> genStmt s
      s : others -> do
        ret <- genStmt s
        (if ret then return True else genBlock $ Block pos others)

-------------------------------------------------

getArgType :: Show a => Arg a -> ValueType
getArgType (Arg _ t _) = genType t

addNeededTerminators :: Bool -> [Instruction] -> [Instruction]
addNeededTerminators ret insts = case insts of
  [] -> [VRetInst]
  _ ->
    ( if ret
        then
          ( case last insts of
              LabelInst _ -> insts ++ [UnreachableInst]
              _ -> insts
          )
        else insts ++ [VRetInst]
    )

-- Statements -----------------------------------------------------------------

-- Result tells if given statement always returns
genStmt :: Show a => Stmt a -> GenM Bool
genStmt x = case x of
  Empty _ -> return False
  BStmt _ block -> runIsolated $ genBlock block
  Decl _ type_ items -> do
    let t = genType type_
    mapM_ (genItem t) items
    return False
  Ass _ ident expr -> assign ident expr >> return False
  Incr _ ident -> do
    var <- getVar ident
    old <- getTempVarVal IntT
    new <- getTempVarVal IntT
    emit $ LoadInst old var
    emit $ AddInst new old $ VarConst IntT $ IntV 1
    emit $ StoreInst new var
    return False
  Decr _ ident -> do
    var <- getVar ident
    old <- getTempVarVal IntT
    new <- getTempVarVal IntT
    emit $ LoadInst old var
    emit $ SubInst new old $ VarConst IntT $ IntV 1
    emit $ StoreInst new var
    return False
  Ret _ expr -> do
    var <- genExpr expr
    emit $ RetInst var
    return True
  VRet _ -> emit VRetInst >> return True
  Cond _ expr stmt -> case tryEvalAsCExpr expr of
    Right (VarConst BoolT (BoolV b), _) -> (if b then runIsolated (genStmt stmt) else return False)
    Left e -> do
      lThen <- freshLabel
      lEnd <- freshLabel
      genNonConstCond e lThen lEnd
      emit $ LabelInst lThen
      runIsolated (genStmt stmt)
      emit $ JumpInst lEnd
      emit $ LabelInst lEnd
      return False
  CondElse _ expr stmt1 stmt2 -> case tryEvalAsCExpr expr of
    Right (VarConst BoolT (BoolV b), _) -> (if b then runIsolated (genStmt stmt1) else runIsolated (genStmt stmt2))
    Left e -> do
      lThen <- freshLabel
      lElse <- freshLabel
      lEnd <- freshLabel
      genNonConstCond expr lThen lElse
      emit $ LabelInst lThen
      ret1 <- runIsolated (genStmt stmt1)
      emit $ JumpInst lEnd
      emit $ LabelInst lElse
      ret2 <- runIsolated (genStmt stmt2)
      emit $ JumpInst lEnd
      emit $ LabelInst lEnd
      return $ ret1 && ret2
  While _ expr stmt -> case tryEvalAsCExpr expr of
    Right (VarConst BoolT (BoolV b), _) ->
      ( if b
          then
            ( do
                lBody <- freshLabel
                emit $ JumpInst lBody
                emit $ LabelInst lBody
                ret <- runIsolated (genStmt stmt)
                emit $ JumpInst lBody
                return ret
            )
          else return False
      )
    Left e -> do
      lBody <- freshLabel
      lCond <- freshLabel
      lEnd <- freshLabel
      emit $ JumpInst lCond
      emit $ LabelInst lBody
      ret <- runIsolated (genStmt stmt)
      emit $ JumpInst lCond
      emit $ LabelInst lCond
      genCond expr lBody lEnd
      emit $ LabelInst lEnd
      return False
  SExp _ expr -> genExpr expr >> return False

genItem :: Show a => ValueType -> Item a -> GenM ()
genItem t x = case x of
  NoInit _ ident -> initialize t ident
  Init _ ident expr -> do
    var <- genExpr expr
    initialize t ident
    assignVal ident var

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

assign :: Show a => Ident -> Expr a -> GenM ()
assign id expr = do
  exprVar <- genExpr expr
  assignVal id exprVar

assignVal :: Ident -> Var -> GenM ()
assignVal id val = do
  var <- getVar id
  case var of
    VarAddr _ _ -> emit $ StoreInst val var
    _ -> setVar id val

-- Expressions ----------------------------------------------------------------

genExpr :: Show a => Expr a -> GenM Var
genExpr = genNonConstExpr . reduceCExprs

genNonConstExpr :: Show a => Expr a -> GenM Var
genNonConstExpr x = case x of
  EVar _ ident -> do
    var <- getVar ident
    case var of
      VarAddr t _ -> do
        tempVar <- getTempVarVal t
        emit $ LoadInst tempVar var
        return tempVar
      _ -> return var
  ELitInt _ integer -> return $ VarConst IntT $ IntV integer
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
  Neg _ expr -> do
    old <- genNonConstExpr expr
    new <- getTempVarVal IntT
    emit $ SubInst new (VarConst IntT $ IntV 0) old
    return new
  EMul _ expr1 mulop expr2 -> do
    let instConstructor = genMulOp mulop
    val1 <- genNonConstExpr expr1
    val2 <- genNonConstExpr expr2
    new <- getTempVarVal IntT
    emit $ instConstructor new val1 val2
    return new
  EAdd _ expr1 addop expr2 -> do
    val1 <- genNonConstExpr expr1
    val2 <- genNonConstExpr expr2
    case varType val1 of
      StrT -> do
        new <- getTempVarVal StrT
        emit $ CallInst new (Ident "concat") [val1, val2]
        return new
      _ -> do
        let instConstructor = genAddOp addop
        new <- getTempVarVal IntT
        emit $ instConstructor new val1 val2
        return new
  ELitTrue _ -> return $ VarConst BoolT $ BoolV True
  ELitFalse _ -> return $ VarConst BoolT $ BoolV False
  cond -> do
    container <- getTempVarAddr BoolT
    res <- getTempVarVal BoolT
    emit $ AllocaInst container
    lThen <- freshLabel
    lElse <- freshLabel
    lAfter <- freshLabel
    genNonConstCond cond lThen lElse
    emit $ LabelInst lThen
    emit $ StoreInst (VarConst BoolT $ BoolV True) container
    emit $ JumpInst lAfter
    emit $ LabelInst lElse
    emit $ StoreInst (VarConst BoolT $ BoolV False) container
    emit $ JumpInst lAfter
    emit $ LabelInst lAfter
    emit $ LoadInst res container
    return res

genCond :: Show a => Expr a -> Label -> Label -> GenM ()
genCond e = genNonConstCond (reduceCExprs e)

genNonConstCond :: Show a => Expr a -> Label -> Label -> GenM ()
genNonConstCond expr lThen lElse = case expr of
  ELitTrue _ -> emit $ JumpInst lThen
  ELitFalse _ -> emit $ JumpInst lElse
  Not _ expr -> genNonConstCond expr lElse lThen
  ERel _ expr1 relop expr2 -> do
    let op = genRelOp relop
    val1 <- genNonConstExpr expr1
    val2 <- genNonConstExpr expr2
    res <- getTempVarVal BoolT
    emit $ ICMPInst res op val1 val2
    emit $ CondJumpInst res lThen lElse
  EAnd _ expr1 expr2 -> do
    lMid <- freshLabel
    genNonConstCond expr1 lMid lElse
    emit $ LabelInst lMid
    genNonConstCond expr2 lThen lElse
  EOr _ expr1 expr2 -> do
    lMid <- freshLabel
    genNonConstCond expr1 lThen lMid
    emit $ LabelInst lMid
    genNonConstCond expr2 lThen lElse
  expr -> do
    val <- genNonConstExpr expr
    emit $ CondJumpInst val lThen lElse

-------------------------------------------------

genAddOp :: Show a => AddOp a -> Var -> Var -> Var -> Instruction
genAddOp x = case x of
  Plus _ -> AddInst
  Minus _ -> SubInst

genMulOp :: Show a => MulOp a -> Var -> Var -> Var -> Instruction
genMulOp x = case x of
  Times _ -> MulInst
  Div _ -> DivInst
  Mod _ -> ModInst

genRelOp :: Show a => RelOp a -> String
genRelOp x = case x of
  LTH _ -> "slt"
  LE _ -> "sle"
  GTH _ -> "sgt"
  GE _ -> "sge"
  EQU _ -> "eq"
  NE _ -> "ne"
