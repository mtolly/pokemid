#!/bin/bash
for asm in *.asm; do
  echo "=>" $asm
  pokemid $asm $asm.mid && pokemid $asm.mid $asm.new.asm
done
