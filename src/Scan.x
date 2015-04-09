{
{-# OPTIONS_GHC -w #-}
module Scan (scan, Token(..), AlexPosn(..)) where

import qualified Assembly
}

%wrapper "posn"

tokens :-

[\ \t]+ ;
\; ([^\n]*) ;

[\n\r] { emit $ const Newline }
\, { emit $ const Comma }

[0-9]+ { emit $ Int . read }
[CDEFGAB][_\#] { emit $ Key . Assembly.readKey }
snare[1-9]      { emit $ Drum . Assembly.readDrum }
triangle[1-3]   { emit $ Drum . Assembly.readDrum }
cymbal[1-3]     { emit $ Drum . Assembly.readDrum }
mutedsnare[1-4] { emit $ Drum . Assembly.readDrum }

rest { emit $ const Rest }
notetype { emit $ const NoteType }
dspeed { emit $ const DSpeed }
octave { emit $ const Octave }
vibrato { emit $ const Vibrato }
duty { emit $ const Duty }
volume { emit $ const Volume }
stereopanning { emit $ const StereoPanning }
pitchbend { emit $ const PitchBend }
tempo { emit $ const Tempo }
loopchannel { emit $ const LoopChannel }
callchannel { emit $ const CallChannel }
endchannel { emit $ const EndChannel }
toggleperfectpitch { emit $ const TogglePerfectPitch }
executemusic { emit $ const ExecuteMusic }
dutycycle { emit $ const DutyCycle }

[A-Za-z0-9_]+ { emit Label }
\: { emit $ const Colon }
\. { emit $ const Dot }

{

emit :: (String -> Token) -> AlexPosn -> String -> (AlexPosn, Token)
emit f pn s = (pn, f s)

data Token
  = Newline
  | Comma
  | Dot
  | Colon
  | Int Int
  | Key Assembly.Key
  | Drum Assembly.Drum
  | Label String
  | Note
  | DNote
  | Rest
  | NoteType
  | DSpeed
  | Octave
  | Vibrato
  | Duty
  | Volume
  | StereoPanning
  | PitchBend
  | Tempo
  | LoopChannel
  | CallChannel
  | EndChannel
  | TogglePerfectPitch
  | ExecuteMusic
  | DutyCycle
  deriving (Eq, Ord, Show, Read)

scan :: String -> [(AlexPosn, Token)]
scan = alexScanTokens

}
