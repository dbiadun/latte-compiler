module PhiGeneration where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import LLVMInstructions
import Types

-- Types ----------------------------------------------------------------------

type PhiM = State PhiState

data PhiState = PhiState
  { activeMap :: Map.Map Label (Set.Set Var),
    defsMap :: Map.Map Label (Set.Set Var),
    inMap :: Map.Map Label (Set.Set Var),
    outMap :: Map.Map Label (Set.Set Var),
    labels :: [Label],
    predMap :: Map.Map Label [Label],
    succMap :: Map.Map Label [Label],
    curLabel :: Maybe Label,
    localActiveSet :: Set.Set Var,
    changed :: Bool,
    storedVars :: Map.Map Var Var,
    vars :: Map.Map Var Var,
    lastStored :: Map.Map (Var, Label) Var
  }

initialState :: PhiState
initialState =
  PhiState
    { activeMap = Map.empty,
      defsMap = Map.empty,
      inMap = Map.empty,
      outMap = Map.empty,
      labels = [],
      predMap = Map.empty,
      succMap = Map.empty,
      curLabel = Nothing,
      localActiveSet = Set.empty,
      changed = False,
      storedVars = Map.empty,
      vars = Map.empty,
      lastStored = Map.empty
    }

-- Operations -----------------------------------------------------------------

stateChangeLabel :: Label -> PhiM ()
stateChangeLabel l = modify (\s -> s {curLabel = Just l})

stateRemoveLabel :: PhiM ()
stateRemoveLabel = modify (\s -> s {curLabel = Nothing})

stateAddLabel :: Label -> PhiM ()
stateAddLabel l = do
  oldLabels <- gets labels
  modify (\s -> s {labels = l : oldLabels})

stateAddPredSuccPair :: Label -> Label -> PhiM ()
stateAddPredSuccPair pred succ = do
  oldPredMap <- gets predMap
  oldSuccMap <- gets succMap
  let predList = Map.findWithDefault [] succ oldPredMap
  let succList = Map.findWithDefault [] pred oldSuccMap
  modify (\s -> s {predMap = Map.insert succ (pred : predList) oldPredMap, succMap = Map.insert pred (succ : succList) oldSuccMap})

stateAddActive :: Label -> Var -> PhiM ()
stateAddActive l v = do
  oldMap <- gets activeMap
  let activeSet = Map.findWithDefault Set.empty l oldMap
  modify (\s -> s {activeMap = Map.insert l (Set.insert v activeSet) oldMap})

stateAddActiveSet :: Label -> Set.Set Var -> PhiM ()
stateAddActiveSet l set = do
  oldMap <- gets activeMap
  modify (\s -> s {activeMap = Map.insert l set oldMap})

stateAddDef :: Label -> Var -> PhiM ()
stateAddDef l v = do
  oldMap <- gets defsMap
  let defs = Map.findWithDefault Set.empty l oldMap
  modify (\s -> s {defsMap = Map.insert l (Set.insert v defs) oldMap})

stateSetIn :: Label -> Set.Set Var -> PhiM ()
stateSetIn l set = do
  oldMap <- gets inMap
  modify (\s -> s {inMap = Map.insert l set oldMap})

stateSetOut :: Label -> Set.Set Var -> PhiM ()
stateSetOut l set = do
  oldMap <- gets outMap
  modify (\s -> s {outMap = Map.insert l set oldMap})

stateGetMapElSet :: Label -> Map.Map Label (Set.Set Var) -> Set.Set Var
stateGetMapElSet = Map.findWithDefault Set.empty

stateSetStoredVar :: Var -> Var -> PhiM ()
stateSetStoredVar v1 v2 = do
  oldMap <- gets storedVars
  modify (\s -> s {storedVars = Map.insert v1 v2 oldMap})

stateSetVar :: Var -> Var -> PhiM ()
stateSetVar v1 v2 = do
  oldMap <- gets vars
  modify (\s -> s {vars = Map.insert v1 v2 oldMap})

stateSetLastStored :: Var -> Label -> Var -> PhiM ()
stateSetLastStored v1 l v2 = do
  oldMap <- gets lastStored
  modify (\s -> s {lastStored = Map.insert (v1, l) v2 oldMap})

stateGetStoredVar :: Var -> PhiM Var
stateGetStoredVar v = gets $ Map.findWithDefault (VarConst IntT (IntV 0)) v . storedVars

stateGetVar :: Var -> PhiM Var
stateGetVar v = gets $ Map.findWithDefault v v . vars

stateGetLastStored :: Var -> Label -> PhiM Var
stateGetLastStored v l = gets $ Map.findWithDefault (VarConst IntT (IntV 0)) (v, l) . lastStored

-------------------------------------------------------------------------------

generatePhi :: [Instruction] -> [Instruction]
generatePhi insts = evalState (generatePhiHelper insts) initialState

generatePhiHelper :: [Instruction] -> PhiM [Instruction]
generatePhiHelper insts = do
  addPredSucc insts
  addLocalActive insts
  addDefs insts
  addGlobalActive
  instsWithProperVars <- replaceVars insts
  addPhi instsWithProperVars

addPredSucc :: [Instruction] -> PhiM ()
addPredSucc insts = stateRemoveLabel >> mapM_ addPredSuccSingle insts

addPredSuccSingle :: Instruction -> PhiM ()
addPredSuccSingle inst = do
  label <- gets curLabel
  case label of
    Just l -> do
      case inst of
        LabelInst newLabel -> stateChangeLabel newLabel >> stateAddLabel newLabel
        JumpInst nextL -> do
          stateAddPredSuccPair l nextL
        CondJumpInst _ l1 l2 -> do
          stateAddPredSuccPair l l1
          stateAddPredSuccPair l l2
        _ -> return ()
    Nothing -> case inst of
      LabelInst newLabel -> stateChangeLabel newLabel >> stateAddLabel newLabel
      _ -> return ()

-------------------------------------------------------------------------------

addLocalActive :: [Instruction] -> PhiM ()
addLocalActive x = case x of
  [] -> return ()
  (inst : rest) -> addLocalActive rest >> addLocalActiveSingle inst

addLocalActiveSingle :: Instruction -> PhiM ()
addLocalActiveSingle inst = case inst of
  LabelInst l -> do
    activeSet <- gets localActiveSet
    stateAddActiveSet l activeSet
    modify (\s -> s {localActiveSet = Set.empty})
  inst -> do
    out <- gets localActiveSet
    let use = usedVars inst
    let kill = declaredVars inst
    let prevAlive = Set.difference out kill
    let alive = Set.union prevAlive use
    modify (\s -> s {localActiveSet = alive})

usedVars :: Instruction -> Set.Set Var
usedVars x = case x of
  LoadInst _ v -> Set.singleton v
  _ -> Set.empty

declaredVars :: Instruction -> Set.Set Var
declaredVars x = case x of
  AllocaInst v -> Set.singleton v
  StoreInst _ cont -> Set.singleton cont
  _ -> Set.empty

-------------------------------------------------------------------------------

addDefs :: [Instruction] -> PhiM ()
addDefs insts = stateRemoveLabel >> mapM_ addDef insts

addDef :: Instruction -> PhiM ()
addDef x = case x of
  LabelInst l -> stateChangeLabel l
  AllocaInst v -> do
    label <- gets curLabel
    case label of
      Just l -> stateAddDef l v
      Nothing -> return ()
  StoreInst _ v -> do
    label <- gets curLabel
    case label of
      Just l -> stateAddDef l v
      Nothing -> return ()
  _ -> return ()

-------------------------------------------------------------------------------

addGlobalActive :: PhiM ()
addGlobalActive = do
  modify (\s -> s {changed = False})
  labels <- gets labels -- fix
  mapM_ addGlobalActiveSingle labels
  changed <- gets changed
  when changed addGlobalActive

addGlobalActiveSingle :: Label -> PhiM ()
addGlobalActiveSingle l = do
  succList <- gets $ Map.lookup l . succMap
  case succList of
    Nothing -> do
      use <- gets $ stateGetMapElSet l . activeMap
      stateSetIn l use
    Just sl -> do
      use <- gets $ stateGetMapElSet l . activeMap
      def <- gets $ stateGetMapElSet l . defsMap
      inStart <- gets $ stateGetMapElSet l . inMap
      outStart <- gets $ stateGetMapElSet l . outMap
      let inSize = Set.size inStart
      let outSize = Set.size outStart
      let outAlive = Set.difference outStart def
      let inSet = Set.union use outAlive
      outSet <- succIns sl
      stateSetIn l inSet
      stateSetOut l outSet
      when (inSize < Set.size inSet || outSize < Set.size outSet) $ modify (\s -> s {changed = True})

succIns :: [Label] -> PhiM (Set.Set Var)
succIns x = case x of
  [] -> return Set.empty
  (l : ls) -> do
    singleIns <- succInsSingle l
    restIns <- succIns ls
    return $ Set.union singleIns restIns

succInsSingle :: Label -> PhiM (Set.Set Var)
succInsSingle l = gets $ stateGetMapElSet l . inMap

-------------------------------------------------------------------------------

replaceVars :: [Instruction] -> PhiM [Instruction]
replaceVars x = case x of
  [] -> return []
  (inst : is) -> do
    newInst <- replaceVarsSingle inst
    rest <- replaceVars is
    case newInst of
      Just i -> return (i : rest)
      Nothing -> return rest

replaceVarsSingle :: Instruction -> PhiM (Maybe Instruction)
replaceVarsSingle x = case x of
  LabelInst l@(Label n) -> do
    stateChangeLabel l
    alive <- gets $ Set.toList . stateGetMapElSet l . inMap
    mapM_ (replaceAliveAddrVar l n) alive
    return $ Just x
  AllocaInst v@(VarAddr t n) -> do
    label <- gets curLabel
    let vNew = VarVal t n
    stateSetStoredVar v vNew
    case label of
      Just l -> stateSetLastStored v l vNew
    return Nothing
  StoreInst val cont -> do
    vNew <- stateGetVar val
    label <- gets curLabel
    stateSetStoredVar cont vNew
    case label of
      Just l -> stateSetLastStored cont l vNew
    return Nothing
  LoadInst var cont -> do
    storedVar <- stateGetStoredVar cont
    stateSetVar var storedVar
    return Nothing
  RetInst v -> do
    vNew <- stateGetVar v
    return $ Just $ RetInst vNew
  CallInst v id vars -> do
    varsNew <- mapM stateGetVar vars
    return $ Just $ CallInst v id varsNew
  AddInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    return $ Just $ AddInst v v1New v2New
  SubInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    return $ Just $ SubInst v v1New v2New
  MulInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    return $ Just $ MulInst v v1New v2New
  DivInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    return $ Just $ DivInst v v1New v2New
  ModInst v v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    return $ Just $ ModInst v v1New v2New
  CondJumpInst v l1 l2 -> do
    vNew <- stateGetVar v
    return $ Just $ CondJumpInst vNew l1 l2
  ICMPInst v s v1 v2 -> do
    v1New <- stateGetVar v1
    v2New <- stateGetVar v2
    return $ Just $ ICMPInst v s v1New v2New
  _ -> return $ Just x

replaceAliveAddrVar :: Label -> Int -> Var -> PhiM ()
replaceAliveAddrVar l n v = do
  let vNew = varAdrToVarStored n v
  stateSetStoredVar v vNew
  stateSetLastStored v l vNew

varAdrToVarStored :: Int -> Var -> Var
varAdrToVarStored i x = case x of
  VarAddr t n -> VarStored t n i
  v -> v

-------------------------------------------------------------------------------

addPhi :: [Instruction] -> PhiM [Instruction]
addPhi insts = do
  instLists <- mapM addPhiSingle insts
  return $ concat instLists

addPhiSingle :: Instruction -> PhiM [Instruction]
addPhiSingle x = case x of
  LabelInst l@(Label id) -> do
    predList <- gets $ Map.findWithDefault [] l . predMap
    aliveVars <- gets $ Set.toList . Map.findWithDefault Set.empty l . inMap
    newInsts <- mapM (addPhiForOneVar id predList) aliveVars
    case predList of
      [] -> return [x]
      _ -> return (x : newInsts)
  _ -> return [x]

addPhiForOneVar :: Int -> [Label] -> Var -> PhiM Instruction
addPhiForOneVar lId preds v = do
  let resVar = varAdrToVarStored lId v
  pairs <- mapM (getPhiPair v) preds
  return $ PhiInst resVar pairs

getPhiPair :: Var -> Label -> PhiM (Var, Label)
getPhiPair v l = do
  newVar <- stateGetLastStored v l
  return (newVar, l)
