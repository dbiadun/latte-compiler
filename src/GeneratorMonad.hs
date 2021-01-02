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

newtype FEnv = FEnv (Map.Map Ident FuncType)

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

freshTemp :: GenM Var
freshTemp = do
  n <- gets nextV
  modify (\s -> s {nextV = n + 1})
  return $ VarAddr n

freshLabel :: GenM Label
freshLabel = do
  n <- gets nextL
  modify (\s -> s {nextL = n + 1})
  return $ Label n

freshAddr :: GenM Var
freshAddr = do
  n <- gets nextAddr
  modify (\s -> s {nextAddr = n + 1})
  return $ VarAddr n

addLocal :: Ident -> GenM ()
addLocal id = do
  v <- freshAddr
  modify
    (\s -> s {venv = VEnv $ runOnVenvMap (Map.insert id $ v) $ venv s})

addConst :: Ident -> Value -> GenM ()
addConst id val =
  modify
    (\s -> s {venv = VEnv $ runOnVenvMap (Map.insert id $ VarConst val) $ venv s})

getVar :: Ident -> GenM Var
getVar id = gets $ runOnVenvMap (Map.findWithDefault (VarConst $ IntV 0) id) . venv

-------------------------------------------------

runOnVenvMap :: (Map.Map Ident Var -> a) -> VEnv -> a
runOnVenvMap f (VEnv map) = f map
