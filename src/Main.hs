module Main where

import Paths_pokemid (version)
import Data.Version (showVersion)
import qualified Scan
import qualified Parse
import qualified Assembly
import qualified Graph
import qualified Midi
import qualified AssemblyToMidi
import qualified MidiToAssembly
import qualified Clean
import qualified Emit
import qualified Error
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
      mid <- Load.fromFile fmid
      let trks = Midi.getTracks mid
          tempoTrack = Error.getTempoTrack mid
          showPosn posn = let
            (m, b) = Error.posnToMeasureBeats tempoTrack posn
            whole :: Int
            (whole, part) = properFraction b
            in "Measure " ++ show (m + 1) ++ ", beat " ++ show (whole + 1) ++
              if part == 0 then "" else ", part " ++ MidiToAssembly.showRat part
      forM_ trks $ \(name, trk) -> let
        chan = Midi.getNamedChannel name
        in case MidiToAssembly.simplify chan trk of
          Left (pos, e) -> error $ showPosn pos ++ ": " ++ e
          Right simple -> let
            (begin, loop) = MidiToAssembly.splitLoop simple
            beginLength = Error.findEnd begin
            asmEvents = (Clean.cleanAssembly <$>) . MidiToAssembly.encode chan
            in case (asmEvents begin, fmap asmEvents loop) of
              (Left (pos, e), _) -> error $ showPosn pos ++ ": " ++ e
              (_, Just (Left (pos, e))) -> error $ showPosn (beginLength + pos) ++ ": " ++ e
              (Right b, Just (Right l)) -> let
                beginLoop = Clean.cleanLoop (b, Just l)
                code = Emit.optimize name beginLoop
                in forM_ code $ putStrLn . Assembly.printAsm
              (Right b, Nothing) -> let
                beginLoop = Clean.cleanLoop (b, Nothing)
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
      hPutStrLn stderr $ prog ++ " v" ++ showVersion version
      hPutStrLn stderr $ "Usage: "++prog++" in.mid > out.asm"
      hPutStrLn stderr $ "       "++prog++" in.asm out.mid"
      exitFailure
