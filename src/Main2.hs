module Main where

import qualified Scan
import qualified Parse
import qualified Midi
import qualified AssemblyGraph
import qualified AssemblyToMidi
import Control.Applicative ((<$>))
import qualified Sound.MIDI.File.Save as Save
import System.Environment (getArgs)
import qualified Data.Map as Map

main :: IO ()
main = do
  [fin, fout] <- getArgs
  graph <- AssemblyGraph.makeGraph . Parse.parse . Scan.scan <$> readFile fin
  let prefix = head
        [ reverse pre
        | s <- Map.keys graph
        , d : 'h' : 'C' : pre <- [reverse s]
        , d `elem` "1234"
        ]
      mid = Midi.fromTracks $ AssemblyToMidi.channelTracks prefix graph
  Save.toFile fout mid
