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
  label { (_, S.Label $$) }
  rest { (_, S.Rest) }
  note_type { (_, S.NoteType) }
  drum_speed { (_, S.DrumSpeed) }
  note { (_, S.Note) }
  drum_note { (_, S.DrumNote) }
  octave { (_, S.Octave) }
  vibrato { (_, S.Vibrato) }
  volume { (_, S.Volume) }
  stereo_panning { (_, S.StereoPanning) }
  pitch_slide { (_, S.PitchSlide) }
  tempo { (_, S.Tempo) }
  sound_loop { (_, S.SoundLoop) }
  sound_call { (_, S.SoundCall) }
  sound_ret { (_, S.SoundRet) }
  toggle_perfect_pitch { (_, S.TogglePerfectPitch) }
  execute_music { (_, S.ExecuteMusic) }
  duty_cycle { (_, S.DutyCycle) }
  duty_cycle_pattern { (_, S.DutyCyclePattern) }

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
     : note key ',' int { Note $2 $4 Nothing }
     | pitch_slide int ',' int ',' key Newlines note key ',' int { Note $9 $11 $ Just ($2, $4, $6) }
     | drum_note int ',' int { DrumNote (toEnum ($2 - 1)) $4 }
     | rest int { Rest $2 }
     | note_type int ',' int ',' int { NoteType $2 $4 $6 }
     | drum_speed int { DrumSpeed $2 }
     | octave int { Octave $2 }
     | vibrato int ',' int ',' int { Vibrato $2 $4 $6 }
     | volume int ',' int { Volume $2 $4 }
     | stereo_panning int ',' int { StereoPanning $2 $4 }
     | tempo int { Tempo $2 }
     | toggle_perfect_pitch { TogglePerfectPitch }
     | execute_music { ExecuteMusic }
     | duty_cycle int { DutyCycle $2 }
     | duty_cycle_pattern int ',' int ',' int ',' int { DutyCyclePattern $2 $4 $6 $8 }

Control :: { Control String }
        : label ':' ':' { Label True $1 }
        | label ':' { Label False $1 }
        | '.' label { LocalLabel $2 }
        | '.' label ':' { LocalLabel $2 }
        | sound_loop int ',' label { SoundLoop $2 $4 }
        | sound_loop int ',' '.' label { SoundLoop $2 $5 }
        | sound_call label { SoundCall $2 }
        | sound_ret { SoundRet }

{

parseError :: [(S.AlexPosn, S.Token)] -> a
parseError [] = error "Parse error at end of file"
parseError ((S.AlexPn _ r c, tok) : _) = error $
  "parse error at line " ++ show r ++ ", column " ++ show c ++ ", token: " ++ show tok

}
