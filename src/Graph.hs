module Graph where

import Assembly
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Block s t
  = Inst (Instruction t) (Block s t)
  | Call s (Block s t)
  | Loop Int s (Block s t)
  | End
  | Goto s
  deriving (Eq, Ord, Show, Read)

type Graph s t = Map.Map s (Block s t)

makeGraph :: Ord s => [Either (Control s) (Instruction t)] -> Graph s t
makeGraph asm = let
  labels = [ s | Left (Label s) <- asm ]
  findLabel l = drop 1 $ dropWhile (not . isLabel l) asm
  isLabel l (Left (Label l')) = l == l'
  isLabel _ _                 = False
  pathToBlock [] = End
  pathToBlock (Left c : rest) = case c of
    Label l -> Goto l
    LoopChannel 0 l -> Goto l
    LoopChannel n l -> Loop n l $ pathToBlock rest
    CallChannel l -> Call l $ pathToBlock rest
    EndChannel -> End
    ToggleCall -> pathToBlock rest -- TODO
  pathToBlock (Right i : rest) = Inst i $ pathToBlock rest
  in Map.fromList $ zip labels $ map (pathToBlock . findLabel) labels

nextLabel :: Block s t -> Maybe s
nextLabel b = case b of
  Inst _ b' -> nextLabel b'
  Call _ b' -> nextLabel b'
  Loop _ _ b' -> nextLabel b'
  End -> Nothing
  Goto l -> Just l

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
