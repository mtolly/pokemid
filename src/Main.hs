module Main where

import qualified Scan
import qualified Parse
import qualified Assembly
import qualified Graph
import qualified Midi
import qualified AssemblyToMidi
import qualified MidiToAssembly
import qualified Clean
import qualified Emit
import Control.Applicative ((<$>))
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import System.Environment (getArgs, getProgName)
import qualified Data.Map as Map
import Control.Monad (forM_)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [fmid] -> do
      trks <- Midi.getTracks <$> Load.fromFile fmid
      forM_ trks $ \(name, trk) -> let
        chan = Midi.getNamedChannel name
        (begin, loop) = MidiToAssembly.splitLoop $ MidiToAssembly.simplify chan trk
        beginLoop = Clean.cleanLoop (asmEvents begin, fmap asmEvents loop)
        asmEvents = Clean.cleanAssembly . MidiToAssembly.encode chan
        code = Emit.optimize name beginLoop
        in forM_ code $ putStrLn . Assembly.printAsm
    [fasm, fmid] -> do
      graph <- Graph.makeGraph . Parse.parse . Scan.scan <$> readFile fasm
      let prefix = head
            [ reverse pre
            | s <- Map.keys graph
            , d : 'h' : 'C' : pre <- [reverse s]
            , d `elem` "1234"
            ]
          mid = Midi.fromTracks $ AssemblyToMidi.channelTracks prefix graph
      Save.toFile fmid mid
    _ -> do
      prog <- getProgName
      hPutStrLn stderr $ "Usage: "++prog++" in.mid > out.asm"
      hPutStrLn stderr $ "       "++prog++" in.asm out.mid"
      exitFailure
