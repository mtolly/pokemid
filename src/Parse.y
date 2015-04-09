{
module Parse (parse) where

import Assembly
import qualified Scan as S
}

%name parse
%tokentype { (S.AlexPosn, S.Token) }
%error { parseError }

%token
  newline { (_, S.Newline) }
  ',' { (_, S.Comma) }
  ':' { (_, S.Colon) }
  '.' { (_, S.Dot) }
  int { (_, S.Int $$) }
  key { (_, S.Key $$) }
  drum { (_, S.Drum $$) }
  label { (_, S.Label $$) }
  rest { (_, S.Rest) }
  notetype { (_, S.NoteType) }
  dspeed { (_, S.DSpeed) }
  octave { (_, S.Octave) }
  vibrato { (_, S.Vibrato) }
  duty { (_, S.Duty) }
  volume { (_, S.Volume) }
  stereopanning { (_, S.StereoPanning) }
  pitchbend { (_, S.PitchBend) }
  tempo { (_, S.Tempo) }
  loopchannel { (_, S.LoopChannel) }
  callchannel { (_, S.CallChannel) }
  endchannel { (_, S.EndChannel) }
  toggleperfectpitch { (_, S.TogglePerfectPitch) }
  executemusic { (_, S.ExecuteMusic) }
  dutycycle { (_, S.DutyCycle) }

%%

-- A file that can optionally start with a newline
File :: { [AsmLine] }
     : Line File1 { $1 : $2 }
     | File1 { $1 }

-- A file that must start with a newline
File1 :: { [AsmLine] }
      : Newlines Line File1 { $2 : $3 }
      | Newlines { [] }
      | { [] }

-- One or more newlines
Newlines : newline { () }
         | newline Newlines { () }

Line :: { AsmLine }
     : Inst { Right $1 }
     | Control { Left $1 }

Inst :: { Instruction Int }
     : key int { Note $1 $2 Nothing }
     | pitchbend int ',' int Newlines key int { Note $6 $7 $ Just ($2, $4) }
     | drum int { DNote $2 $1 }
     | rest int { Rest $2 }
     | notetype int ',' int ',' int { NoteType $2 $4 $6 }
     | dspeed int { DSpeed $2 }
     | octave int { Octave $2 }
     | vibrato int ',' int ',' int { Vibrato $2 $4 $6 }
     | duty int { Duty $2 }
     | volume int ',' int { Volume $2 $4 }
     | stereopanning int { StereoPanning $2 }
     | tempo int { Tempo $2 }
     | toggleperfectpitch { TogglePerfectPitch }
     | executemusic { ExecuteMusic }
     | dutycycle int { DutyCycle $2 }

Control :: { Control String }
        : label ':' ':' { Label $1 }
        | label ':' { Label $1 }
        | '.' label { LocalLabel $2 }
        | '.' label ':' { LocalLabel $2 }
        | loopchannel int ',' label { LoopChannel $2 $4 }
        | callchannel label { CallChannel $2 }
        | endchannel { EndChannel }

{

parseError :: [(S.AlexPosn, S.Token)] -> a
parseError [] = error "Parse error at end of file"
parseError ((S.AlexPn _ r c, tok) : _) = error $
  "parse error at line " ++ show r ++ ", column " ++ show c ++ ", token: " ++ show tok

}
