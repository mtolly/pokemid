# Pokémon Red/Blue MIDI importer

This is a tool for the [Pokémon Red/Blue disassembly project][pokered], which
allows importing MIDI files into the game's music engine to replace existing
tracks.

[pokered]: https://github.com/iimarckus/pokered

Features:

  * Given a monophonic sequence of MIDI notes, takes care of generating `octave`
    commands and using the right combination of `notetype` speeds and `note`
    lengths to encode MIDI durations.

  * Can create one-shot or looping tracks.

  * Changes tempo with MIDI tempo events.

  * Supports the following note modifiers: `notetype`, `pitchbend`, `vibrato`,
    `duty`, and `stereopanning`.

  * Finds some repeated sections of assembly events and breaks them out into
    subroutines, called with `callchannel`, to save ROM space.

Future work:

  * Understand/support `unknownmusic0xee`, an event used only in the Rocket
    HQ theme (for the odd fade-in notes at the start).

  * Extend to the [pokecrystal][] project?

[pokecrystal]: https://github.com/kanzure/pokecrystal

  * Better algorithm for finding subroutines, and possibly using `loopchannel`
    to further shorten the assembly code.

  * Better error reporting: errors should have MIDI positions and better
    explanations.

## Usage

    pokemid in.mid > out.asm

The MIDI file should be in the following format:

  * Up to 4 tracks, each for one Game Boy channel, where the track for channel
    `N` (1 to 4) has a track name event containing `ChN`.

  * MIDI tempos are used to create tempo change events, inserted into the first
    track in the assembly file.

  * In addition to monophonic notes, you can insert engine commands which affect
    the sound using MIDI text events. Supported events are:

      * `notetype <volume>, <fade>`
      * `vibrato <delay>, <rate>, <depth>`
      * `duty <int>`
      * `stereopanning <int>`
      * `pitchbend <int>, <int>`

    For documentation on these see the [pokered] project.
    All of these events affect all notes that come after them, except the
    `pitchbend` event which affects only the next (or coinciding) note.

    Note that `notetype` does not require the first parameter, though you can
    supply it if you wish. Note encoding speeds are chosen by the program
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
