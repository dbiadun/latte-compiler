module GeneratorMonad where

import AbsLatte
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import LLVMInstructions
import Types

-- Types ----------------------------------------------------------------------

type Result = GenM ()

type GenM = StateT GenState (Writer [Instruction])

data GenState = GenState
  { fenv :: FEnv,
    venv :: VEnv,
    slenv :: SLEnv,
    nextL :: Int,
    nextAddr :: Int
  }

-------------------------------------------------

newtype FEnv = FEnv (Map.Map Ident ValueType)

newtype VEnv = VEnv (Map.Map Ident Var)

newtype SLEnv = SLEnv (Map.Map String StrLiteral)

-- Run ------------------------------------------------------------------------

runGenM :: GenState -> GenM a -> (GenState, [Instruction], a)
runGenM startState monad =
  let writerMonad = runStateT monad startState -- monad with result of type (a, GenState)
      ((ret, state), instructions) = runWriter writerMonad
   in (state, instructions, ret)

getInstructions :: GenM () -> GenM [Instruction]
getInstructions m = do
  state <- get
  let (_, insts, _) = runGenM state m
  return insts

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
      slenv = SLEnv Map.empty,
      nextL = 0,
      nextAddr = 0
    }

-- Operations -----------------------------------------------------------------

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

setVar :: Ident -> Var -> GenM ()
setVar id var = modify (\s -> s {venv = VEnv $ runOnVenvMap (Map.insert id var) $ venv s})

addStrLiteral :: String -> GenM StrLiteral
addStrLiteral s = do
  sl <- gets $ runOnSLenvMap (Map.lookup s) . slenv
  case sl of
    Just ret -> return ret
    Nothing -> do
      n <- gets $ runOnSLenvMap Map.size . slenv
      let ret = StrLiteral n (1 + length s) $ showLiteral s
      modify
        (\state -> state {slenv = SLEnv $ runOnSLenvMap (Map.insert s ret) $ slenv state})
      return ret

getVar :: Ident -> GenM Var
getVar id = gets $ runOnVenvMap (Map.findWithDefault (VarConst IntT $ IntV 0) id) . venv

getTempVarAddr :: ValueType -> GenM Var
getTempVarAddr t = VarAddr t <$> freshAddr

getTempVarVal :: ValueType -> GenM Var
getTempVarVal t = VarVal t <$> freshAddr

addFunction :: Ident -> ValueType -> GenM ()
addFunction id t = modify (\s -> s {fenv = FEnv $ runOnFenvMap (Map.insert id t) $ fenv s})

getFunctionType :: Ident -> GenM ValueType
getFunctionType id = gets $ runOnFenvMap (Map.findWithDefault VoidT id) . fenv

emit :: Instruction -> GenM ()
emit inst = lift $ tell [inst]

emitMultiple :: [Instruction] -> GenM ()
emitMultiple insts = lift $ tell insts

modifyInstructions :: (a -> [Instruction] -> [Instruction]) -> GenM a -> GenM ()
modifyInstructions f m = do
  state <- get
  let (nextState, insts, ret) = runGenM state m
  put nextState
  emitMultiple $ f ret insts

-------------------------------------------------

runOnVenvMap :: (Map.Map Ident Var -> a) -> VEnv -> a
runOnVenvMap f (VEnv map) = f map

runOnFenvMap :: (Map.Map Ident ValueType -> a) -> FEnv -> a
runOnFenvMap f (FEnv map) = f map

runOnSLenvMap :: (Map.Map String StrLiteral -> a) -> SLEnv -> a
runOnSLenvMap f (SLEnv map) = f map
