{- |
A control-flow graph representation of the music assembly code, which functions
as an intermediate form on the way to a 'LoopForm'.
-}
module Graph where

import Assembly
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- | A basic block which ends in an optional branch.
data Block s t
  = Inst (Instruction t) (Block s t)
  | Call s (Block s t)
  | Loop Int s (Block s t)
  | End
  | Goto s
  deriving (Eq, Ord, Show, Read)

type Graph s t = Map.Map s (Block s t)

-- | Rewrites local labels into global ones, with a prefix appended per scope.
localToGlobal
  :: [Either (Control String) (Instruction t)]
  -> [Either (Control String) (Instruction t)]
localToGlobal insts = let
  scopes = splitScopes insts
  splitScopes [] = []
  splitScopes (x : xs) = case break isGlobal xs of
    (s, rest) -> (x : s) : splitScopes rest
  isGlobal (Left (Label _ _)) = True
  isGlobal _                  = False
  fixScope (s, i) = let
    -- For each scope, find all the local labels inside it.
    -- A local label should shadow a global label with the same name.
    locals = [ l | Left (LocalLabel l) <- s ]
    in flip map s $ \e -> case e of
      Left (LocalLabel lbl) -> Left $ Label False $ fromLocal lbl
      Left c                -> Left $ flip fmap c $ \lbl ->
        if lbl `elem` locals then fromLocal lbl else lbl
      Right inst            -> Right inst
      where fromLocal lbl = "localscope" ++ show i ++ "_" ++ lbl
  in concatMap fixScope $ zip scopes ([0..] :: [Int])

-- | Converts the raw assembly sequence into a mapping from labels to blocks.
-- Will crash if given local labels; use localToGlobal first.
makeGraph :: Ord s => [Either (Control s) (Instruction t)] -> Graph s t
makeGraph asm = let
  labels = [ s | Left (Label _ s) <- asm ]
  findLabel l = drop 1 $ dropWhile (not . isLabel l) asm
  isLabel l (Left (Label _ l')) = l == l'
  isLabel _ _                 = False
  pathToBlock [] = End
  pathToBlock (Left c : rest) = case c of
    Label _ l -> Goto l
    LocalLabel _ -> error
      "makeGraph: panic! encountered a local label. call localToGlobal first"
    LoopChannel 0 l -> Goto l
    LoopChannel n l -> Loop n l $ pathToBlock rest
    CallChannel l -> Call l $ pathToBlock rest
    EndChannel -> End
  pathToBlock (Right i : rest) = Inst i $ pathToBlock rest
  in Map.fromList $ zip labels $ map (pathToBlock . findLabel) labels

nextLabel :: Block s t -> Maybe s
nextLabel b = case b of
  Inst _ b' -> nextLabel b'
  Call _ b' -> nextLabel b'
  Loop _ _ b' -> nextLabel b'
  End -> Nothing
  Goto l -> Just l

{- |
\"Runs\" the music graph to convert one channel into a simple 'LoopForm'.
Small subloops are expanded out, so we get one or two simple blocks of
instructions with no branches.
-}
loopForm :: (Show s, Ord s) => s -> Graph s t -> LoopForm t
loopForm start g = let
  getBlock lbl = fromMaybe
    (error $ "loopForm: no such label in graph " ++ show start)
    (Map.lookup lbl g)
  firstLoop = go (Set.singleton start) start
  go labelsSeen lbl = do
    next <- nextLabel $ getBlock lbl
    if Set.member next labelsSeen
      then Just next
      else go (Set.insert next labelsSeen) next
  instList stopLabel loopCounter returnTo b = case b of
    Inst i b' -> i : instList stopLabel loopCounter returnTo b'
    Call l b' -> instList stopLabel loopCounter (Just b') (getBlock l)
    Loop i l b' -> if loopCounter == i
      then instList stopLabel 1 returnTo b'
      else instList stopLabel (loopCounter + 1) returnTo (getBlock l)
    End -> case returnTo of
      Nothing -> []
      Just b' -> instList stopLabel loopCounter Nothing b'
    Goto l -> if stopLabel == Just l
      then []
      else instList stopLabel loopCounter returnTo $ getBlock l
  blockFinite    = instList Nothing    1 Nothing
  blockUntil lbl = instList (Just lbl) 1 Nothing
  in case firstLoop of
    Nothing  -> (blockFinite    $ getBlock start, Nothing)
    Just lbl -> (blockUntil lbl $ getBlock start, Just $ blockUntil lbl $ getBlock lbl)
