module EmitAssembly where

import Assembly

optimize ::
  String -> ([Instruction Int], Maybe [Instruction Int]) -> [AsmInstruction]
optimize name (begin, loop) = concat
  [ [Left $ Label name]
  , map Right begin
  , case loop of
    Nothing -> [Left EndChannel]
    Just l  -> concat
      [ [Left $ Label loopName]
      , map Right l
      , [Left $ LoopChannel 0 loopName]
      ] where loopName = name ++ "_loop"
  ]
  -- TODO: condense code
