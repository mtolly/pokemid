# Pokémon Red/Blue MIDI importer

This is a tool for the [Pokémon Red/Blue disassembly project][pokered], which
allows importing MIDI files into the game's music engine to replace existing
tracks.

[pokered]: https://github.com/iimarckus/pokered

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

  * Only tempos that fit into the following equation are precisely encodable:

        x = 19200 / bpm

    where `x` is a 2-byte integer, and `bpm` is the tempo in beats per minute.

  * Only note durations that fit into the following equation are precisely
    encodable:

        duration = (ticks / 4) * (speed / 12)
        duration = (ticks * speed) / 48

    where `duration` is the length of the note in beats or quarter notes,
    and `ticks` and `speed` are numbers in the range 1 to 15. If a length cannot
    be exactly represented, the closest one less than it will be used.
