module SemanticAnalysis where

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

data Value = IntV Integer | StrV String | BoolV Bool deriving (Eq)

data FuncType = FuncType ValueType [ValueType]

newtype FEnv = FEnv (Map.Map Ident FuncType)

newtype TEnv = TEnv (Map.Map Ident ValueType)

newtype LocTEnv = LocTEnv (Set.Set Ident)

data SAState = SAState
  { fenv :: FEnv,
    tenv :: TEnv,
    locTenv :: LocTEnv,
    returnType :: ValueType,
    returned :: Bool
  }

-------------------------------------------------------------------------------

check :: Show a => Program a -> IO ()
check p = do
  let (ret, s) = runState (runExceptT $ checkProgram p) initialState
  case ret of
    Left err -> do
      hPutStrLn stderr "ERROR"
      hPutStrLn stderr err
      exitFailure
    _ -> hPutStrLn stderr "OK"

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
      returned = False
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
  when (type_ == VoidT || type_ == NoneT) $ staticCheckError pos "Incorrect variable type"
  modify $ addVariableType id type_

-- Functions ------------------------------------------------------------------

addFunction :: Ident -> FuncType -> SAM ()
addFunction id f =
  let addFunctionToState :: Ident -> FuncType -> SAState -> SAState
      addFunctionToState id1 f1 s =
        s {fenv = FEnv $ runOnFenvMap (Map.insert id1 f1) $ fenv s}
   in modify $ addFunctionToState id f

addPredefinedFunctions :: SAM ()
addPredefinedFunctions = do
  addFunction (Ident "printInt") (FuncType VoidT [IntT])
  addFunction (Ident "printString") (FuncType VoidT [StrT])
  addFunction (Ident "error") (FuncType VoidT [])
  addFunction (Ident "readInt") (FuncType IntT [])
  addFunction (Ident "readString") (FuncType StrT [])

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
  addPredefinedFunctions
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
      checkBlock block
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

checkBlock :: Show a => Block a -> SAM ()
checkBlock x = case x of
  Block _ stmts -> checkAllStmts stmts

checkAllStmts :: Show a => [Stmt a] -> SAM ()
checkAllStmts x = case x of
  [] -> return ()
  (s : stmts) -> checkStmt s >> checkAllStmts stmts

checkStmt :: Show a => Stmt a -> SAM ()
checkStmt x = case x of
  Empty _ -> return ()
  BStmt _ block -> do
    returned <- runIsolated $ do
      checkBlock block
      gets returned
    when returned $ setReturned True
  Decl pos type_ items -> checkAllItems (getValueType type_) items
  Ass pos ident expr -> do
    varType <- getType pos ident
    (exprType, _) <- checkExpr expr
    when (exprType /= varType) $ staticCheckError pos "Expression type not matched with variable type"
  Incr pos ident -> do
    varType <- getType pos ident
    when (varType /= IntT) $ staticCheckError pos "Incrementation of non-integer value"
  Decr pos ident -> do
    varType <- getType pos ident
    when (varType /= IntT) $ staticCheckError pos "Decrementation of non-integer value"
  Ret pos expr -> do
    returnType <- gets returnType
    when (returnType == VoidT) $ staticCheckError pos "Non-void return statement inside a void function"
    (exprType, _) <- checkExpr expr
    when (exprType /= returnType) $ staticCheckError pos "Returning value of incorrect type"
    setReturned True
  VRet pos -> do
    returnType <- gets returnType
    when (returnType /= VoidT) $ staticCheckError pos "Void return statement inside a non-void function"
    setReturned True
  Cond pos expr stmt -> do
    (exprType, exprValue) <- checkExpr expr
    when (exprType /= BoolT) $ staticCheckError pos "Non-boolean condition"
    returned <- runIsolated $ do
      checkStmt stmt
      gets returned
    when (returned && (exprValue == Just (BoolV True))) $ setReturned True
  CondElse pos expr stmt1 stmt2 -> do
    (exprType, exprValue) <- checkExpr expr
    when (exprType /= BoolT) $ staticCheckError pos "Non-boolean condition"
    ret1 <- runIsolated $ do
      checkStmt stmt1
      gets returned
    ret2 <- runIsolated $ do
      checkStmt stmt2
      gets returned
    when (ret1 && (exprValue == Just (BoolV True))) $ setReturned True
    when (ret2 && (exprValue == Just (BoolV False))) $ setReturned True
    when (ret1 && ret2) $ setReturned True
  While pos expr stmt -> do
    (exprType, exprValue) <- checkExpr expr
    when (exprType /= BoolT) $ staticCheckError pos "Non-boolean condition"
    returned <- runIsolated $ do
      checkStmt stmt
      gets returned
    when (returned && (exprValue == Just (BoolV True))) $ setReturned True
  SExp pos expr -> void $ checkExpr expr

checkAllItems :: Show a => ValueType -> [Item a] -> SAM ()
checkAllItems t items = case items of
  [] -> return ()
  (i : is) -> checkItem t i >> checkAllItems t is

checkItem :: Show a => ValueType -> Item a -> SAM ()
checkItem t x = case x of
  NoInit pos ident -> declareVariable pos ident t
  Init pos ident expr -> do
    (exprType, _) <- checkExpr expr
    when (exprType /= t) $ staticCheckError pos "Expression type not matched with variable type"
    declareVariable pos ident t

-- Expressions ----------------------------------------------------------------

checkExpr :: Show a => Expr a -> SAM (ValueType, Maybe Value)
checkExpr x = case x of
  EVar pos ident -> do
    t <- getType pos ident
    return (t, Nothing)
  ELitInt _ integer -> return (IntT, Just (IntV integer))
  ELitTrue _ -> return (BoolT, Just (BoolV True))
  ELitFalse _ -> return (BoolT, Just (BoolV False))
  EApp pos ident exprs -> do
    (FuncType retType argTypes) <- getFunction pos ident
    checkArgsFromExprs pos argTypes exprs
    return (retType, Nothing)
  EString _ string -> return (StrT, Just (StrV string))
  Neg pos expr -> do
    (exprT, exprV) <- checkExpr expr
    unless (exprT == IntT) $
      staticCheckError pos "Trying to use '-' operator on non-integer value"
    return (IntT, evalIntOpOne negate exprV)
  Not pos expr -> do
    (exprT, exprV) <- checkExpr expr
    unless (exprT == BoolT) $
      staticCheckError pos "Trying to use '!' operator on non-bool value"
    return (BoolT, evalBoolOpOne not exprV)
  EMul _ expr1 mulop expr2 -> do
    let pos = getMulOpPos mulop
    (t1, v1) <- checkExpr expr1
    unless (t1 == IntT) $
      staticCheckError pos $
        "First argument of '"
          ++ getMulOp mulop
          ++ "' operator is not an integer value"
    (t2, v2) <- checkExpr expr2
    unless (t2 == IntT) $
      staticCheckError pos $
        "Second argument of '"
          ++ getMulOp mulop
          ++ "' operator is not an integer value"
    case mulop of
      Times _ -> return (IntT, evalIntOpTwo (*) v1 v2)
      Div pos -> case v2 of
        Just (IntV 0) -> staticCheckError pos "Division by 0"
        _ -> return (IntT, evalIntOpTwo div v1 v2)
      Mod pos -> case v2 of
        Just (IntV 0) -> staticCheckError pos "Modulo operation with 0 base"
        _ -> return (IntT, evalIntOpTwo mod v1 v2)
  EAdd _ expr1 addop expr2 -> case addop of
    Plus pos -> do
      (t1, v1) <- checkExpr expr1
      unless (t1 == IntT || t1 == StrT) $
        staticCheckError pos $
          "First argument of '"
            ++ getAddOp addop
            ++ "' operator is not an integer nor string value"
      (t2, v2) <- checkExpr expr2
      unless (t2 == IntT || t2 == StrT) $
        staticCheckError pos $
          "Second argument of '"
            ++ getAddOp addop
            ++ "' operator is not an integer nor string value"
      case (t1, t2) of
        (IntT, IntT) -> return (IntT, evalIntOpTwo (+) v1 v2)
        (StrT, StrT) -> return (StrT, evalStringOp (++) v1 v2)
        _ -> staticCheckError pos "Trying to add integer and string values"
    Minus pos -> do
      (t1, v1) <- checkExpr expr1
      unless (t1 == IntT) $
        staticCheckError pos $
          "First argument of '"
            ++ getAddOp addop
            ++ "' operator is not an integer value"
      (t2, v2) <- checkExpr expr2
      unless (t2 == IntT) $
        staticCheckError pos $
          "Second argument of '"
            ++ getAddOp addop
            ++ "' operator is not an integer value"
      return (IntT, evalIntOpTwo (-) v1 v2)
  ERel _ expr1 relop expr2 -> do
    let pos = getRelOpPos relop
    (t1, v1) <- checkExpr expr1
    (t2, v2) <- checkExpr expr2
    unless (t1 == t2) $ staticCheckError pos "Comparing values of different types"
    case relop of
      EQU _ -> return (BoolT, evalRelOp (==) v1 v2)
      NE _ -> return (BoolT, evalRelOp (/=) v1 v2)
      _ -> do
        unless (t1 == IntT) $
          staticCheckError pos $
            "First argument of '"
              ++ getRelOp relop
              ++ "' operator is not an integer value"
        unless (t2 == IntT) $
          staticCheckError pos $
            "Second argument of '"
              ++ getRelOp relop
              ++ "' operator is not an integer value"
        case relop of
          LTH _ -> return (BoolT, evalIntRelOp (<) v1 v2)
          LE _ -> return (BoolT, evalIntRelOp (<=) v1 v2)
          GTH _ -> return (BoolT, evalIntRelOp (>) v1 v2)
          GE _ -> return (BoolT, evalIntRelOp (>=) v1 v2)
  EAnd pos expr1 expr2 -> do
    (t1, v1) <- checkExpr expr1
    unless (t1 == BoolT) $
      staticCheckError
        pos
        "First argument of '&&' operator is not a bool value"
    (t2, v2) <- checkExpr expr2
    unless (t2 == BoolT) $
      staticCheckError
        pos
        "Second argument of '&&' operator is not a bool value"
    return (BoolT, evalBoolOpTwo (&&) v1 v2)
  EOr pos expr1 expr2 -> do
    (t1, v1) <- checkExpr expr1
    unless (t1 == BoolT) $
      staticCheckError
        pos
        "First argument of '|| operator is not a bool value"
    (t2, v2) <- checkExpr expr2
    unless (t2 == BoolT) $
      staticCheckError
        pos
        "Second argument of '||' operator is not a bool value"
    return (BoolT, evalBoolOpTwo (||) v1 v2)

evalBoolOpTwo :: (Bool -> Bool -> Bool) -> Maybe Value -> Maybe Value -> Maybe Value
evalBoolOpTwo op v1 v2 = case (v1, v2) of
  (Just (BoolV n1), Just (BoolV n2)) -> Just $ BoolV $ op n1 n2
  _ -> Nothing

evalBoolOpOne :: (Bool -> Bool) -> Maybe Value -> Maybe Value
evalBoolOpOne op v = case v of
  Just (BoolV n) -> Just $ BoolV $ op n
  _ -> Nothing

evalIntOpTwo :: (Integer -> Integer -> Integer) -> Maybe Value -> Maybe Value -> Maybe Value
evalIntOpTwo op v1 v2 = case (v1, v2) of
  (Just (IntV n1), Just (IntV n2)) -> Just $ IntV $ op n1 n2
  _ -> Nothing

evalIntRelOp :: (Integer -> Integer -> Bool) -> Maybe Value -> Maybe Value -> Maybe Value
evalIntRelOp op v1 v2 = case (v1, v2) of
  (Just (IntV n1), Just (IntV n2)) -> Just $ BoolV $ op n1 n2
  _ -> Nothing

evalIntOpOne :: (Integer -> Integer) -> Maybe Value -> Maybe Value
evalIntOpOne op v = case v of
  Just (IntV n) -> Just $ IntV $ op n
  _ -> Nothing

evalStringOp :: (String -> String -> String) -> Maybe Value -> Maybe Value -> Maybe Value
evalStringOp op v1 v2 = case (v1, v2) of
  (Just (StrV s1), Just (StrV s2)) -> Just $ StrV $ op s1 s2
  _ -> Nothing

evalRelOp :: (Maybe Value -> Maybe Value -> Bool) -> Maybe Value -> Maybe Value -> Maybe Value
evalRelOp op v1 v2 = case (v1, v2) of
  (Just (BoolV _), Just (BoolV _)) -> Just $ BoolV $ op v1 v2
  (Just (IntV _), Just (IntV _)) -> Just $ BoolV $ op v1 v2
  (Just (StrV _), Just (StrV _)) -> Just $ BoolV $ op v1 v2
  _ -> Nothing

exprPos :: Show a => Expr a -> a
exprPos x = case x of
  EVar pos _ -> pos
  ELitInt pos _ -> pos
  ELitTrue pos -> pos
  ELitFalse pos -> pos
  EApp pos _ _ -> pos
  EString pos _ -> pos
  Neg pos _ -> pos
  Not pos _ -> pos
  EMul pos _ _ _ -> pos
  EAdd pos _ _ _ -> pos
  ERel pos _ _ _ -> pos
  EAnd pos _ _ -> pos
  EOr pos _ _ -> pos

checkArgsFromExprs :: Show a => a -> [ValueType] -> [Expr a] -> SAM ()
checkArgsFromExprs _ [] [] = return ()
checkArgsFromExprs pos (type_ : types) (expr : exprs) = do
  nextArgs <- checkArgsFromExprs pos types exprs
  (exprT, _) <- checkExpr expr
  unless (exprT == type_) $ staticCheckError (exprPos expr) "Incorrect type of argument"
checkArgsFromExprs pos _ _ =
  staticCheckError pos "Incorrect number of arguments"

-- Operators ------------------------------------------------------------------

getAddOp :: Show a => AddOp a -> String
getAddOp x = case x of
  Plus _ -> "+"
  Minus _ -> "-"

getMulOp :: Show a => MulOp a -> String
getMulOp x = case x of
  Times _ -> "*"
  Div _ -> "/"
  Mod _ -> "%"

getRelOp :: Show a => RelOp a -> String
getRelOp x = case x of
  LTH _ -> "<"
  LE _ -> "<="
  GTH _ -> ">"
  GE _ -> ">="
  EQU _ -> "=="
  NE _ -> "!="

getMulOpPos :: Show a => MulOp a -> a
getMulOpPos x = case x of
  Times pos -> pos
  Div pos -> pos
  Mod pos -> pos

getRelOpPos :: Show a => RelOp a -> a
getRelOpPos x = case x of
  LTH pos -> pos
  LE pos -> pos
  GTH pos -> pos
  GE pos -> pos
  EQU pos -> pos
  NE pos -> pos
