module LCSE where

import AbsLatte
import Control.Monad.State
import qualified Data.Map as Map
import LLVMInstructions
import Types

-- Types ----------------------------------------------------------------------

type LCSEM = State LCSEState

data LCSEState = LCSEState
  { storedVars :: Map.Map RHS Var,
    vars :: Map.Map Var Var
  }

initialState :: LCSEState
initialState =
  LCSEState
    { storedVars = Map.empty,
      vars = Map.empty
    }

data RHS
  = AddRHS Var Var
  | SubRHS Var Var
  | MulRHS Var Var
  | DivRHS Var Var
  | ModRHS Var Var
  | ICMPRHS String Var Var
  | AssignStrLiteralRHS StrLiteral
  deriving (Eq, Ord)

-- Operations -----------------------------------------------------------------

getRHS :: Instruction -> RHS
getRHS x = case x of
  AddInst _ v1 v2 -> AddRHS v1 v2
  SubInst _ v1 v2 -> SubRHS v1 v2
  MulInst _ v1 v2 -> MulRHS v1 v2
  DivInst _ v1 v2 -> DivRHS v1 v2
  ModInst _ v1 v2 -> ModRHS v1 v2
  ICMPInst _ s v1 v2 -> ICMPRHS s v1 v2
  AssignStrLiteral _ sl -> AssignStrLiteralRHS sl

stateSetStoredVar :: RHS -> Var -> LCSEM ()
stateSetStoredVar r v = do
  oldMap <- gets storedVars
  modify (\s -> s {storedVars = Map.insert r v oldMap})

stateSetVar :: Var -> Var -> LCSEM ()
stateSetVar v1 v2 = do
  oldMap <- gets vars
  modify (\s -> s {vars = Map.insert v1 v2 oldMap})

stateGetStoredVar :: RHS -> LCSEM (Maybe Var)
stateGetStoredVar r = gets $ Map.lookup r . storedVars

stateGetVar :: Var -> LCSEM Var
stateGetVar v = gets $ Map.findWithDefault v v . vars

-------------------------------------------------------------------------------

performLCSE :: [Instruction] -> [Instruction]
performLCSE insts = evalState (performLCSEHelper insts) initialState

performLCSEHelper :: [Instruction] -> LCSEM [Instruction]
performLCSEHelper x = case x of
  [] -> return []
  (inst : is) -> do
    newInst <- performLCSESingle inst
    rest <- performLCSEHelper is
    case newInst of
      Just i -> return (i : rest)
      Nothing -> return rest

-------------------------------------------------------------------------------

performLCSESingle :: Instruction -> LCSEM (Maybe Instruction)
performLCSESingle x = case x of
  RetInst v -> do
    vNew <- stateGetVar v
    return $ Just $ RetInst vNew
  CallInst v id vars -> do
    varsNew <- mapM stateGetVar vars
    return $ Just $ CallInst v id varsNew
  AddInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    let instNew = AddInst v v1New v2New
    performLCSESingleOnProperInst instNew v
  SubInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    let instNew = SubInst v v1New v2New
    performLCSESingleOnProperInst instNew v
  MulInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    let instNew = MulInst v v1New v2New
    performLCSESingleOnProperInst instNew v
  DivInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    let instNew = DivInst v v1New v2New
    performLCSESingleOnProperInst instNew v
  ModInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    let instNew = ModInst v v1New v2New
    performLCSESingleOnProperInst instNew v
  CondJumpInst v l1 l2 -> do
    vNew <- stateGetVar v
    return $ Just $ CondJumpInst vNew l1 l2
  ICMPInst v s v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    let instNew = ICMPInst v s v1New v2New
    performLCSESingleOnProperInst instNew v
  AssignStrLiteral v sl -> do
    vNew <- stateGetVar v
    let instNew = AssignStrLiteral vNew sl
    performLCSESingleOnProperInst instNew v
  LabelInst _ -> do
    modify (\s -> s {storedVars = Map.empty})
    return $ Just x
  PhiInst v varsAndLabels -> do
    varsNew <- mapM (stateGetVar . fst) varsAndLabels
    let labels = map snd varsAndLabels
    return $ Just $ PhiInst v $ zip varsNew labels
  _ -> return $ Just x

performLCSESingleOnProperInst :: Instruction -> Var -> LCSEM (Maybe Instruction)
performLCSESingleOnProperInst instNew v = do
  let rhs = getRHS instNew
  lhs <- stateGetStoredVar rhs
  case lhs of
    Just l -> do
      stateSetVar v l
      return Nothing
    Nothing -> do
      stateSetStoredVar rhs v
      return $ Just instNew
