module Main where

import qualified Scan
import qualified Parse
import qualified Midi
import qualified AssemblyGraph
import qualified AssemblyToMidi
import Control.Applicative ((<$>))
import qualified Sound.MIDI.File.Save as Save
import System.Environment (getArgs)

main :: IO ()
main = do
  [prefix, fin, fout] <- getArgs
  graph <- AssemblyGraph.makeGraph . Parse.parse . Scan.scan <$> readFile fin
  let mid = Midi.fromTracks $ AssemblyToMidi.channelTracks prefix graph
  Save.toFile fout mid
