# Pokémon Red/Blue MIDI music converter

[![Build Status](https://travis-ci.org/mtolly/pokemid.svg?branch=master)](https://travis-ci.org/mtolly/pokemid)

This is a tool for the [Pokémon Red/Blue disassembly project][pokered], which
allows editing the music tracks by exporting them to MIDI files and then
importing them back into assembly.

[pokered]: https://github.com/iimarckus/pokered

Features:

  * Given a monophonic sequence of MIDI notes, takes care of generating `octave`
    commands and using the right combination of `notetype` speeds and `note`
    lengths to encode MIDI durations.

  * Can create one-shot or looping tracks.

  * Changes tempo with MIDI tempo events.

  * Supports the following note modifiers: `notetype`, `pitchbend`, `vibrato`,
    `duty`, `volume`, `stereopanning`, and `toggleperfectpitch`.

  * Finds repeated sections of assembly events and breaks them out into
    subroutines, called with `callchannel`, to save ROM space.

Future work:

  * Extend to the [pokecrystal][] project?

[pokecrystal]: https://github.com/kanzure/pokecrystal

  * Possibly use `loopchannel` to further shorten the assembly code.

## Build

Windows/Mac/Linux executables are posted on the [releases] page. To build the
latest code yourself:

[releases]: https://github.com/mtolly/pokemid/releases

  1. Install the [Haskell Platform], or (for advanced users)
    [GHC] + [Alex] + [Happy] + [`cabal-install`][cabal].
    GHC 7.4, 7.6, 7.8, and 7.10 are tested.

[Haskell Platform]: https://www.haskell.org/platform/
[GHC]: https://www.haskell.org/ghc/
[Alex]: https://www.haskell.org/alex/
[Happy]: https://www.haskell.org/happy/
[cabal]: https://www.haskell.org/haskellwiki/Cabal-Install

  2. `cabal update` if necessary to download package lists.

  3. In the `pokemid` directory,
    `cabal sandbox init` if you want the installed dependencies sandboxed (recommended).

  4. `cabal install` to build and install.
    Or, `cabal build` and then move `dist/build/pokemid/pokemid(.exe)`
    to the directory of your choice.

## Usage

    pokemid in.asm out.mid # assembly to MIDI
    pokemid in.mid out.asm # MIDI to assembly
    # Second argument can be elided to go to stdout

For MIDI to assembly, the MIDI file should be in the following format:

  * Up to 4 tracks, each for one Game Boy channel, where the track for channel
    `N` (1 to 4) has a track name event containing `ChN`.

  * MIDI tempos are used to create tempo change events, inserted into the first
    track in the assembly file.

  * In addition to monophonic notes, you can insert engine commands which affect
    the sound using MIDI text events. Supported events are:

      * `notetype <volume>, <fade>`
      * `vibrato <delay>, <rate>, <depth>`
      * `duty <int>`
      * `volume <left>, <right>`
      * `stereopanning <int>`
      * `pitchbend <int>, <int>`
      * `toggleperfectpitch`

    For documentation on these see the [pokered] project.
    All of these events affect all notes that come after them, except the
    `pitchbend` event which affects only the next (or coinciding) note.

    Note that `notetype` does not require you to supply a note encoding
    speed, though you can if you wish (before the volume and fade
    parameters). It is ignored because speeds are chosen by the program
    automatically.

    Events cannot be placed in the middle of a note, only at its beginning/end
    or in the middle of a rest.

  * To make a loop, use text events `begin` and `end` around the looping
    portion. Without these the song will simply play once. You can also have
    just an `end` event for a one-shot song. `begin` and `end`, like other
    events, cannot be in the middle of a note.

  * Only tempos that fit into the following equation are precisely encodable:

        x = 19200 / bpm

    where `x` is a 2-byte integer, and `bpm` is the tempo in beats per minute.
    `x` will be rounded to the nearest integer if needed.

  * Only note durations that fit into the following equation are precisely
    encodable:

        duration = (ticks / 4) * (speed / 12)
        duration = (ticks * speed) / 48

    where `duration` is the length of the note in beats or quarter notes,
    `ticks` is a number from 1 to 16, and `speed` is a number from 1 to 15.
    Furthermore, `ticks` and `speed` cannot both be 1.
    If a length cannot be exactly represented, the closest one less than it will
    be used. Rests between notes/events can be any length which is a sum of
    encodable lengths.

  * Channel 4 notes (percussion/noise) use the following pitches:

    * 0: `snare1`
    * 1: `snare2`
    * 2: `snare3`
    * 3: `snare4`
    * 4: `snare5`
    * 5: `triangle1`
    * 6: `triangle2`
    * 7: `snare6`
    * 8: `snare7`
    * 9: `snare8`
    * 10: `snare9`
    * 11: `cymbal1`
    * 12: `cymbal2`
    * 13: `cymbal3`
    * 14: `mutedsnare1`
    * 15: `triangle3`
    * 16: `mutedsnare2`
    * 17: `mutedsnare3`
    * 18: `mutedsnare4`

  * Assembly `notetype` volumes are encoded as MIDI note velocities as follows:

    * Assembly 0: MIDI 1 to 7
    * Assembly 1: MIDI 8 to 15
    * Assembly 2: MIDI 16 to 23
    * Assembly 3: MIDI 24 to 31
    * Assembly 4: MIDI 32 to 39
    * Assembly 5: MIDI 40 to 47
    * Assembly 6: MIDI 48 to 55
    * Assembly 7: MIDI 56 to 63
    * Assembly 8: MIDI 64 to 71
    * Assembly 9: MIDI 72 to 79
    * Assembly 10: MIDI 80 to 87
    * Assembly 11: MIDI 88 to 95
    * Assembly 12: MIDI 96 to 103
    * Assembly 13: MIDI 104 to 111
    * Assembly 14: MIDI 112 to 119
    * Assembly 15: MIDI 120 to 127

    That is, each assembly volume gets 8 MIDI velocities, except assembly 0 which gets 7.
