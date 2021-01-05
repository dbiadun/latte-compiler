module GeneratorMonad where

import AbsLatte
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import ErrM
import LLVMInstructions
import Types

-- Types ----------------------------------------------------------------------

type Result = GenM ()

type GenM = ExceptT ErrorType (StateT GenState (Writer [Instruction]))

newtype ErrorType = VariableNotDefined Ident deriving (Show)

data GenState = GenState
  { fenv :: FEnv,
    venv :: VEnv,
    lenv :: LEnv,
    nextV :: Int,
    nextL :: Int,
    nextAddr :: Int
  }

-------------------------------------------------

newtype FEnv = FEnv (Map.Map Ident ValueType)

newtype VEnv = VEnv (Map.Map Ident Var)

newtype LEnv = LEnv (Map.Map String Literal)

-- Run ------------------------------------------------------------------------

runGenM :: GenState -> GenM () -> Either ErrorType (GenState, [Instruction])
runGenM initialState monad = do
  let stateMonad = runExceptT monad -- monad with result of type Either ErrorType ()
  let writerMonad = runStateT stateMonad initialState -- monad with result of type (Either ErrorType (), GenState)
  let ((ok, state), instructions) = runWriter writerMonad
  ok
  return (state, instructions)

runIsolated :: GenM a -> GenM a
runIsolated monad = do
  venv <- gets venv
  ret <- monad
  modify (\s -> s {venv = venv})
  return ret

-- State ----------------------------------------------------------------------

initialState :: GenState
initialState =
  GenState
    { fenv = FEnv Map.empty,
      venv = VEnv Map.empty,
      lenv = LEnv Map.empty,
      nextV = 0,
      nextL = 0,
      nextAddr = 0
    }

-- Operations -----------------------------------------------------------------

freshTemp :: ValueType -> GenM Var
freshTemp t = do
  n <- gets nextV
  modify (\s -> s {nextV = n + 1})
  return $ VarAddr t n

freshLabel :: GenM Label
freshLabel = do
  n <- gets nextL
  modify (\s -> s {nextL = n + 1})
  return $ Label n

freshAddr :: GenM Int
freshAddr = do
  n <- gets nextAddr
  modify (\s -> s {nextAddr = n + 1})
  return n

addVarAddr :: ValueType -> Ident -> GenM Var
addVarAddr t id = do
  n <- freshAddr
  let v = VarAddr t n
  modify
    (\s -> s {venv = VEnv $ runOnVenvMap (Map.insert id v) $ venv s})
  return v

addVarVal :: ValueType -> Ident -> GenM Var
addVarVal t id = do
  n <- freshAddr
  let v = VarVal t n
  modify
    (\s -> s {venv = VEnv $ runOnVenvMap (Map.insert id v) $ venv s})
  return v

addConst :: ValueType -> Ident -> Value -> GenM Var
addConst t id val = do
  let v = VarConst t val
  modify
    (\s -> s {venv = VEnv $ runOnVenvMap (Map.insert id v) $ venv s})
  return v

getVar :: Ident -> GenM Var
getVar id = gets $ runOnVenvMap (Map.findWithDefault (VarConst IntT $ IntV 0) id) . venv

getTempVarVal :: ValueType -> GenM Var
getTempVarVal t = VarVal t <$> freshAddr

addFunction :: Ident -> ValueType -> GenM ()
addFunction id t = modify (\s -> s {fenv = FEnv $ runOnFenvMap (Map.insert id t) $ fenv s})

getFunctionType :: Ident -> GenM ValueType
getFunctionType id = gets $ runOnFenvMap (Map.findWithDefault VoidT id) . fenv

emit :: Instruction -> GenM ()
emit inst = lift $ lift $ tell [inst]

-------------------------------------------------

runOnVenvMap :: (Map.Map Ident Var -> a) -> VEnv -> a
runOnVenvMap f (VEnv map) = f map

runOnFenvMap :: (Map.Map Ident ValueType -> a) -> FEnv -> a
runOnFenvMap f (FEnv map) = f map
