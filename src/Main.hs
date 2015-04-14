{-# LANGUAGE CPP #-}
module Main (main) where

import Paths_pokemid (version)
import Data.Version (showVersion)
import System.Environment (getArgs, getProgName)

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

import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import Sound.MIDI.Parser.Report (T(..))

import System.IO (stderr, hPutStrLn, withFile, IOMode(..))
import System.Exit (exitFailure)
import Control.Exception (evaluate)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8

midToAsm :: B.ByteString -> IO String
midToAsm bs = do
  mid <- case Load.maybeFromByteString $ BL.fromStrict bs of
    Cons _ (Right mid) -> return mid
    err                -> error $ show err
  let trks = Midi.getTracks mid
      tempoTrack = Error.getTempoTrack mid
      showPosn posn = let
        (m, b) = Error.posnToMeasureBeats tempoTrack posn
        whole :: Int
        (whole, part) = properFraction b
        in "Measure " ++ show (m + 1) ++ ", beat " ++ show (whole + 1) ++
          if part == 0 then "" else ", part " ++ MidiToAssembly.showRat part
      codeLines = flip concatMap trks $ \(name, trk) -> let
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
                in map Assembly.printAsm code
              (Right b, Nothing) -> let
                beginLoop = Clean.cleanLoop (b, Nothing)
                code = Emit.optimize name beginLoop
                in map Assembly.printAsm code
      codeStr = unlines codeLines
  _ <- evaluate $ length codeStr
  return codeStr

asmToMid :: String -> IO B.ByteString
asmToMid str = let
  graph = Graph.makeGraph $ Graph.localToGlobal $ Parse.parse $ Scan.scan str
  prefix = head
    [ reverse pre
    | s <- Map.keys graph
    , d : 'h' : 'C' : pre <- [reverse s]
    , d `elem` "1234"
    ]
  mid = Midi.fromTracks $ AssemblyToMidi.channelTracks prefix graph
  bs = BL.toStrict $ Save.toByteString mid
  in do
    _ <- evaluate bs
    return bs

isMidi :: FilePath -> IO Bool
isMidi fp = withFile fp ReadMode $ \h -> do
  b <- B.hGet h 4
  return $ b == B8.pack "MThd"

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  mapM_ (hPutStrLn stderr)
    [ prog ++ " v" ++ showVersion version
    , "Usage: "++prog++" in.mid > out.asm"
    , "       "++prog++" in.asm > out.mid"
    , "       "++prog++" in.mid out.asm"
    , "       "++prog++" in.asm out.mid"
    ]

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [f1] -> do
      m2a <- isMidi f1
      if m2a
        then B.readFile f1 >>= midToAsm >>= putStr
        else readFile f1 >>= asmToMid >>= B.putStr
    [f1, f2] -> do
      m2a <- isMidi f1
      if m2a
        then B.readFile f1 >>= midToAsm >>= writeFile f2
        else readFile f1 >>= asmToMid >>= B.writeFile f2
    _ -> printUsage >> exitFailure
