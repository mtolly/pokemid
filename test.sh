#!/bin/bash
set -e
set -u

for asm in test/*.asm; do
  echo " => $asm"
  echo "asm -> mid"
  dist/build/pokemid/pokemid $asm $asm.mid
  echo "mid -> asm"
  dist/build/pokemid/pokemid $asm.mid $asm.new.asm
  rm $asm.mid
  rm $asm.new.asm
done
