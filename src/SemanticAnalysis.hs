module SemanticAnalysis where

-- Haskell module generated by the BNF converter

import AbsLatte as Grammar
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import ErrM
import System.Exit (exitFailure)
import System.IO

-- Types ----------------------------------------------------------------------

type Result = SAM (Err String)

type SAM = ExceptT String (State SAState)

data ValueType = IntT | StrT | BoolT | VoidT | NoneT deriving (Eq)

data FuncType = FuncType ValueType [ValueType]

newtype FEnv = FEnv (Map.Map Ident FuncType)

newtype TEnv = TEnv (Map.Map Ident ValueType)

newtype LocTEnv = LocTEnv (Set.Set Ident)

data SAState = SAState
  { fenv :: FEnv,
    tenv :: TEnv,
    locTenv :: LocTEnv,
    returnType :: ValueType,
    returned :: Bool,
    inLoopOrIf :: Bool
  }

-------------------------------------------------------------------------------

check :: Show a => Program a -> IO ()
check p = do
  let (ret, s) = runState (runExceptT $ checkProgram p) initialState
  case ret of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    _ -> return ()

-- Except ---------------------------------------------------------------------

showPos :: Show a => a -> String
showPos pos = case show pos of
  ('J' : 'u' : 's' : 't' : ' ' : realPos) -> realPos
  _ -> show pos

staticCheckError :: Show a => a -> String -> SAM b
staticCheckError pos mes =
  throwError $ "StaticError at " ++ showPos pos ++ ": " ++ mes

-- State ----------------------------------------------------------------------

initialState :: SAState
initialState =
  SAState
    { fenv = FEnv Map.empty,
      tenv = TEnv Map.empty,
      locTenv = LocTEnv Set.empty,
      returnType = NoneT,
      returned = False,
      inLoopOrIf = False
    }

runOnFenvMap :: (Map.Map Ident FuncType -> a) -> FEnv -> a
runOnFenvMap f (FEnv map) = f map

runOnTenvMap :: (Map.Map Ident ValueType -> a) -> TEnv -> a
runOnTenvMap f (TEnv map) = f map

runOnLocTenvMap :: (Set.Set Ident -> a) -> LocTEnv -> a
runOnLocTenvMap f (LocTEnv set) = f set

runIsolated :: SAM a -> SAM a
runIsolated x = do
  formerState <- get
  modify (\s -> s {locTenv = LocTEnv Set.empty})
  ret <- x
  put formerState
  return ret

-- Variables ------------------------------------------------------------------

isDeclared :: Ident -> SAM Bool
isDeclared id = gets $ runOnTenvMap (Map.member id) . tenv

isDeclaredLocally :: Ident -> SAM Bool
isDeclaredLocally id = gets $ runOnLocTenvMap (Set.member id) . locTenv

getType :: Show a => a -> Ident -> SAM ValueType
getType pos id = do
  maybeType <- gets $ runOnTenvMap (Map.lookup id) . tenv
  case maybeType of
    Just t -> return t
    Nothing ->
      staticCheckError pos ("Variable " ++ showId id ++ " not declared.")

addVariableType :: Ident -> ValueType -> SAState -> SAState
addVariableType id t s =
  s
    { tenv = TEnv $ runOnTenvMap (Map.insert id t) $ tenv s,
      locTenv = LocTEnv $ runOnLocTenvMap (Set.insert id) $ locTenv s
    }

declareVariable :: Show a => a -> Ident -> ValueType -> SAM ()
declareVariable pos id type_ = do
  declared <- isDeclaredLocally id
  when declared $ staticCheckError pos $ "Variable " ++ showId id ++ " declared more than once at local scope"
  modify $ addVariableType id type_

-- Functions ------------------------------------------------------------------

addFunction :: Ident -> FuncType -> SAM ()
addFunction id f =
  let addFunctionToState :: Ident -> FuncType -> SAState -> SAState
      addFunctionToState id1 f1 s =
        s {fenv = FEnv $ runOnFenvMap (Map.insert id1 f1) $ fenv s}
   in modify $ addFunctionToState id f

getFunction :: Show a => a -> Ident -> SAM FuncType
getFunction pos id = do
  maybeF <- gets $ runOnFenvMap (Map.lookup id) . fenv
  case maybeF of
    Just f -> return f
    Nothing -> staticCheckError pos $ "Function " ++ showId id ++ " not defined"

functionDeclared :: Ident -> SAM Bool
functionDeclared id = gets $ runOnFenvMap (Map.member id) . fenv

changeReturnType :: ValueType -> SAM ()
changeReturnType t = modify (\s -> s {returnType = t})

setReturned :: Bool -> SAM ()
setReturned b = modify (\s -> s {returned = b})

setInLoopOrIf :: Bool -> SAM ()
setInLoopOrIf b = modify (\s -> s {inLoopOrIf = b})

-- Helpers --------------------------------------------------------------------

showId :: Ident -> String
showId (Ident id) = id

failure :: Show a => a -> Result
failure x = return $ Bad $ "Undefined case: " ++ show x

checkIdent :: Ident -> Result
checkIdent x = case x of
  Ident string -> failure x

-- Program --------------------------------------------------------------------

checkProgram :: Show a => Program a -> SAM ()
checkProgram (Program pos defs) = do
  declareFunctions defs
  checkDefs defs
  getFunction (Just (0, 0)) (Ident "main")
  return ()

declareFunctions :: Show a => [TopDef a] -> SAM ()
declareFunctions defs = case defs of
  [] -> return ()
  (d : ds) -> declareFunction d >> declareFunctions ds

declareFunction :: Show a => TopDef a -> SAM ()
declareFunction (FnDef pos type_ id args block) = do
  alreadyDeclared <- functionDeclared id
  ( if alreadyDeclared
      then staticCheckError pos $ "Redefinition of function " ++ showId id
      else
        ( do
            let returnType = getValueType type_
            let argTypes = map getArgType args
            when (id == Ident "main") $ do
              when (returnType /= IntT) $ staticCheckError pos "Incorrect type of function main"
              when (argTypes /= []) $ staticCheckError pos "Function main should have no arguments"
            addFunction id $ FuncType returnType argTypes
        )
    )

getValueType :: Show a => Type a -> ValueType
getValueType t = case t of
  Grammar.Int _ -> IntT
  Grammar.Str _ -> StrT
  Grammar.Bool _ -> BoolT
  Grammar.Void _ -> VoidT
  _ -> NoneT

getArgType :: Show a => Arg a -> ValueType
getArgType (Arg _ t _) = getValueType t

-- Definitions ----------------------------------------------------------------

checkDefs :: Show a => [TopDef a] -> SAM ()
checkDefs x = case x of
  [] -> return ()
  (d : defs) -> checkTopDef d >> checkDefs defs

checkTopDef :: Show a => TopDef a -> SAM ()
checkTopDef x = case x of
  FnDef pos type_ ident args block -> do
    let returnType = getValueType type_
    runIsolated $ do
      changeReturnType returnType
      checkArgs args
      --      checkBlock block
      returned <- gets returned
      when (returnType /= VoidT && not returned) $ staticCheckError pos $ "Missing return statement in function " ++ showId ident

checkArgs :: Show a => [Arg a] -> SAM ()
checkArgs x = case x of
  [] -> return ()
  (a : args) -> checkArg a >> checkArgs args

checkArg :: Show a => Arg a -> SAM ()
checkArg x = case x of
  Arg pos type_ ident -> do
    let t = getValueType type_
    declareVariable pos ident t

-- Statements -----------------------------------------------------------------

checkBlock :: Show a => Block a -> Result
checkBlock x = case x of
  Block _ stmts -> failure x

checkStmt :: Show a => Stmt a -> Result
checkStmt x = case x of
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

checkItem :: Show a => Item a -> Result
checkItem x = case x of
  NoInit _ ident -> failure x
  Init _ ident expr -> failure x

checkType :: Show a => Type a -> Result
checkType x = case x of
  Int _ -> failure x
  Str _ -> failure x
  Bool _ -> failure x
  Void _ -> failure x
  Fun _ type_ types -> failure x

checkExpr :: Show a => Expr a -> Result
checkExpr x = case x of
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

checkAddOp :: Show a => AddOp a -> Result
checkAddOp x = case x of
  Plus _ -> failure x
  Minus _ -> failure x

checkMulOp :: Show a => MulOp a -> Result
checkMulOp x = case x of
  Times _ -> failure x
  Div _ -> failure x
  Mod _ -> failure x

checkRelOp :: Show a => RelOp a -> Result
checkRelOp x = case x of
  LTH _ -> failure x
  LE _ -> failure x
  GTH _ -> failure x
  GE _ -> failure x
  EQU _ -> failure x
  NE _ -> failure x
