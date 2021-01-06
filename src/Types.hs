module Types where

import AbsLatte
import Data.Char (isControl, ord)
import Data.List (intercalate)
import Text.Printf (printf)

-------------------------------------------------------------------------------

data FuncType = FuncType ValueType [ValueType]

data FuncHeader = FuncHeader ValueType Ident [FuncArg]

data Var = VarAddr ValueType Int | VarVal ValueType Int | VarConst ValueType Value | VarSize Int

data StrLiteral = StrLiteral Int Int String

newtype Label = Label Int

newtype Register = Register Int deriving (Show)

-------------------------------------------------

data ValueType = IntT | StrT | BoolT | VoidT | NoneT

newtype FuncArg = FuncArg Var

data Value = IntV Integer | StrV String | BoolV Bool

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

instance Show StrLiteral where
  show (StrLiteral id size str) = "[" ++ show size ++ " x i8]* @sl_" ++ show id

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

instance ShowSimple StrLiteral where
  showSimple (StrLiteral id size str) = "@sl_" ++ show id

-- More printing --------------------------------------------------------------

showLiteral :: String -> String
showLiteral s =
  let showChar :: Char -> String
      showChar c = (if isControl c then printf "\\%02X\\00" $ ord c else [c])
   in concatMap showChar s

showLiteralType :: StrLiteral -> String
showLiteralType (StrLiteral _ size _) = "[" ++ show size ++ " x i8]"

-- Operations -----------------------------------------------------------------

varType :: Var -> ValueType
varType x = case x of
  VarAddr t _ -> t
  VarVal t _ -> t
  VarConst t _ -> t
  VarSize _ -> IntT

sizeVar :: Var -> Var
sizeVar x = case x of
  VarAddr _ n -> VarSize n
  VarVal _ n -> VarSize n
  VarConst _ _ -> VarSize 0
  VarSize n -> VarSize n
