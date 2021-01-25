module Types where

import AbsLatte
import Data.Char (isControl, ord)
import Data.List (intercalate)
import Text.Printf (printf)

-------------------------------------------------------------------------------

data FuncType = FuncType ValueType [ValueType]

data FuncHeader = FuncHeader ValueType Ident [FuncArg]

data Var = VarAddr ValueType Int | VarVal ValueType Int | VarConst ValueType Value | VarSize Int | VarStored ValueType Int Int deriving (Eq, Ord)

data StrLiteral = StrLiteral Int Int String deriving (Eq, Ord)

newtype Label = Label Int deriving (Eq, Ord)

newtype Register = Register Int deriving (Show)

-------------------------------------------------

data ValueType = IntT | StrT | BoolT | VoidT | NoneT deriving (Eq, Ord)

newtype FuncArg = FuncArg Var

data Value = IntV Integer | StrV String | BoolV Bool deriving (Eq, Ord)

-- Constant expressions -------------------------------------------------------

data CExpr a
  = CELitInt Integer
  | CELitTrue
  | CELitFalse
  | CENeg Var
  | CENot Var
  | CEMul Var (MulOp a) Var
  | CEAdd Var (AddOp a) Var
  | CERel Var (RelOp a) Var
  | CEAnd Var Var
  | CEOr Var Var

-- Show -----------------------------------------------------------------------

instance Show FuncHeader where
  show (FuncHeader type_ id args) =
    "define " ++ show type_ ++ " @" ++ showId id ++ "("
      ++ intercalate ", " (map show args)
      ++ ") {"

instance Show Var where
  show (VarAddr t n) = show t ++ "* %va_" ++ show n
  show (VarVal t n) = show t ++ " %vv_" ++ show n
  show (VarConst t v) = show t ++ " " ++ show v
  show (VarSize n) = "i32 %s_" ++ show n
  show (VarStored t n1 n2) = show t ++ " %vv_" ++ show n1 ++ "_" ++ show n2

instance Show StrLiteral where
  show (StrLiteral id size str) = "[" ++ show size ++ " x i8]* @sl_" ++ show id

instance Show Label where
  show (Label id) = "label %l" ++ show id

-------------------------------------------------

instance Show ValueType where
  show IntT = "i32"
  show StrT = "i8*"
  show BoolT = "i1"
  show VoidT = "void"
  show NoneT = "none"

instance Show FuncArg where
  show (FuncArg var) = show var

instance Show Value where
  show (IntV n) = show n
  show (StrV s) = s
  show (BoolV b) = if b then "true" else "false"

showId :: Ident -> String
showId (Ident s) = s

-- ShowSimple -----------------------------------------------------------------

class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple Var where
  showSimple (VarAddr _ n) = "%va_" ++ show n
  showSimple (VarVal _ n) = "%vv_" ++ show n
  showSimple (VarConst _ v) = show v
  showSimple (VarSize n) = "%s_" ++ show n
  showSimple (VarStored _ n1 n2) = "%vv_" ++ show n1 ++ "_" ++ show n2

instance ShowSimple StrLiteral where
  showSimple (StrLiteral id size str) = "@sl_" ++ show id

instance ShowSimple Label where
  showSimple (Label id) = "l" ++ show id ++ ":"

-- More printing --------------------------------------------------------------

showLiteral :: String -> String
showLiteral s =
  let showChar :: Char -> String
      showChar c = (if isControl c || ord c < 40 then printf "\\%02X" $ ord c else [c])
   in concatMap showChar (s ++ "\0")

showLiteralType :: StrLiteral -> String
showLiteralType (StrLiteral _ size _) = "[" ++ show size ++ " x i8]"

showLabelAlone :: Label -> String
showLabelAlone (Label id) = "%l" ++ show id

-- Operations -----------------------------------------------------------------

varType :: Var -> ValueType
varType x = case x of
  VarAddr t _ -> t
  VarVal t _ -> t
  VarConst t _ -> t
  VarSize _ -> IntT
  VarStored t _ _ -> t

sizeVar :: Var -> Var
sizeVar x = case x of
  VarAddr _ n -> VarSize n
  VarVal _ n -> VarSize n
  VarConst _ _ -> VarSize 0
  VarSize n -> VarSize n
  VarStored _ n _ -> VarSize n

defaultExpr :: Show a => a -> ValueType -> Expr a
defaultExpr pos t = case t of
  IntT -> ELitInt pos 0
  StrT -> EString pos "\"\""
  BoolT -> ELitFalse pos
  _ -> ELitInt pos 0

-- Contant expression operations ----------------------------------------------

isConst :: Var -> Bool
isConst var = case var of
  VarConst _ _ -> True
  _ -> False

reduceCExprs :: Show a => Expr a -> Expr a
reduceCExprs = toExpr . tryEvalAsCExpr

tryEvalAsCExpr :: Show a => Expr a -> Either (Expr a) (Var, a)
tryEvalAsCExpr x = case x of
  EVar _ _ -> Left x
  ELitInt pos n -> return $ evalCExpr pos $ CELitInt n
  ELitTrue pos -> return $ evalCExpr pos CELitTrue
  ELitFalse pos -> return $ evalCExpr pos CELitFalse
  EApp {} -> Left x
  EString _ _ -> Left x
  Neg pos expr -> do
    case tryEvalAsCExpr expr of
      Left e -> Left $ Neg pos e
      Right (v, _) -> return $ evalCExpr pos $ CENeg v
  Not pos expr -> do
    case tryEvalAsCExpr expr of
      Left e -> Left $ Not pos e
      Right (v, _) -> return $ evalCExpr pos $ CENot v
  EMul pos expr1 op expr2 -> do
    let x1 = tryEvalAsCExpr expr1
    let x2 = tryEvalAsCExpr expr2
    case (x1, x2) of
      (Right (v1, _), Right (v2, _)) -> return $ evalCExpr pos $ CEMul v1 op v2
      _ -> Left $ EMul pos (toExpr x1) op (toExpr x2)
  EAdd pos expr1 op expr2 -> do
    let x1 = tryEvalAsCExpr expr1
    let x2 = tryEvalAsCExpr expr2
    case (x1, x2) of
      (Right (v1, _), Right (v2, _)) -> return $ evalCExpr pos $ CEAdd v1 op v2
      _ -> Left $ EAdd pos (toExpr x1) op (toExpr x2)
  ERel pos expr1 op expr2 -> do
    let x1 = tryEvalAsCExpr expr1
    let x2 = tryEvalAsCExpr expr2
    case (x1, x2) of
      (Right (v1, _), Right (v2, _)) -> return $ evalCExpr pos $ CERel v1 op v2
      _ -> Left $ ERel pos (toExpr x1) op (toExpr x2)
  EAnd pos expr1 expr2 -> do
    let x1 = tryEvalAsCExpr expr1
    let x2 = tryEvalAsCExpr expr2
    case (x1, x2) of
      (Right f@(VarConst BoolT (BoolV False), _), _) -> return f
      (Right (v1, _), Right (v2, _)) -> return $ evalCExpr pos $ CEAnd v1 v2
      _ -> Left $ EAnd pos (toExpr x1) (toExpr x2)
  EOr pos expr1 expr2 -> do
    let x1 = tryEvalAsCExpr expr1
    let x2 = tryEvalAsCExpr expr2
    case (x1, x2) of
      (Right t@(VarConst BoolT (BoolV True), _), _) -> return t
      (Right (v1, _), Right (v2, _)) -> return $ evalCExpr pos $ CEOr v1 v2
      _ -> Left $ EOr pos (toExpr x1) (toExpr x2)

evalCExpr :: Show a => a -> CExpr a -> (Var, a)
evalCExpr pos x =
  let val = case x of
        CELitInt n -> VarConst IntT $ IntV n
        CELitTrue -> VarConst BoolT $ BoolV True
        CELitFalse -> VarConst BoolT $ BoolV False
        CENeg v -> VarConst IntT $ IntV $ negate $ getIntVal v
        CENot v -> VarConst BoolT $ BoolV $ not $ getBoolVal v
        CEMul v1 op v2 -> VarConst IntT $ IntV $ getMulOp op (getIntVal v1) (getIntVal v2)
        CEAdd v1 op v2 -> VarConst IntT $ IntV $ getAddOp op (getIntVal v1) (getIntVal v2)
        CERel v1 op v2 -> case varType v1 of
          IntT -> VarConst BoolT $ BoolV $ getIntRelOp op (getIntVal v1) (getIntVal v2)
          BoolT -> VarConst BoolT $ BoolV $ getBoolRelOp op (getBoolVal v1) (getBoolVal v2)
          _ -> VarConst IntT $ IntV 0
        CEAnd v1 v2 -> VarConst BoolT $ BoolV $ getBoolVal v1 && getBoolVal v2
        CEOr v1 v2 -> VarConst BoolT $ BoolV $ getBoolVal v1 || getBoolVal v2
   in (val, pos)

-------------------------------------------------

toExpr :: Show a => Either (Expr a) (Var, a) -> Expr a
toExpr x = case x of
  Left expr -> expr
  Right (VarConst IntT (IntV n), pos) -> ELitInt pos n
  Right (VarConst BoolT (BoolV b), pos) -> (if b then ELitTrue pos else ELitFalse pos)

getIntVal :: Var -> Integer
getIntVal v = case v of
  VarConst IntT (IntV n) -> n
  _ -> 0

getBoolVal :: Var -> Bool
getBoolVal v = case v of
  VarConst BoolT (BoolV b) -> b
  _ -> False

getAddOp :: Show a => AddOp a -> Integer -> Integer -> Integer
getAddOp x = case x of
  Plus _ -> (+)
  Minus _ -> (-)

getMulOp :: Show a => MulOp a -> Integer -> Integer -> Integer
getMulOp x = case x of
  Times _ -> (*)
  Div _ -> div
  Mod _ -> rem

getIntRelOp :: Show a => RelOp a -> Integer -> Integer -> Bool
getIntRelOp x = case x of
  LTH _ -> (<)
  LE _ -> (<=)
  GTH _ -> (>)
  GE _ -> (>=)
  EQU _ -> (==)
  NE _ -> (/=)

getBoolRelOp :: Show a => RelOp a -> Bool -> Bool -> Bool
getBoolRelOp x = case x of
  LTH _ -> (\_ _ -> False)
  LE _ -> (\_ _ -> False)
  GTH _ -> (\_ _ -> False)
  GE _ -> (\_ _ -> False)
  EQU _ -> (==)
  NE _ -> (/=)
