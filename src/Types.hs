module Types where

-------------------------------------------------------------------------------

data FuncType = FuncType ValueType [ValueType]

data Var = VarAddr Int | VarConst Value deriving (Show)

newtype Literal = Literal Int

newtype Label = Label Int

newtype Register = Register Int deriving (Show)

-------------------------------------------------

data ValueType = IntT | StrT | BoolT | VoidT | NoneT deriving (Eq)

data Value = IntV Integer | StrV String | BoolV Bool deriving (Eq, Show)
