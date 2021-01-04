module Types where

import AbsLatte
import Data.List (intercalate)

-------------------------------------------------------------------------------

data FuncType = FuncType ValueType [ValueType]

data FuncHeader = FuncHeader ValueType Ident [FuncArg]

data Var = VarAddr ValueType Int | VarVal ValueType Int | VarConst ValueType Value

newtype Literal = Literal Int

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