
# Bytebeat DSL
Compiles tracker-style music DSL to a javascript expression.
Written in Haskell.

## Usage
`cabal run song.js example/*`

Creates a file called `song.js`, copy and paste into **[this bytebeat site](https://greggman.com/downloads/examples/html5bytebeat/html5bytebeat.html)** and render at the appropriate sample rate (the example song is compiled to 32kHz).

## Features
* Text tracker format:
```
%samples 44100
%mix 1.5 1.4 1.8 1.8 1.8 1.0 1.0
%master 0.35

# epic song

=
D#4 A0.15 S10.0 W100 H3 Z1.0 0.1 sn
E-4 A0.25 S00.0
.
A-4
.
D-5
---
```
* Sine, Square, Saw waveforms (combinable)
* Noise
* Layering harmonics
* Harmonic amplitude modulation
* Fequency modulation
* Vibrato, pitch slide
* Channel mixing
* Key transposition
* Reverb
* Any oscillator can use sine, square, or saw.

## Format Reference
Each channel is a sequence of steps, empty lines divide the channels.
### Effects
* `A float` -- amplitude
* `D float` -- dampen amplitude per step (can be negative)
* `S float` -- add value to frequency per step (can be negative)
* `H int` -- number of harmonics
* `V float float waveform` -- vibrato frequency/depth/waveform
* `Z float float waveform` -- frequency modulation, first argument is ratio to current playing frequency to modulate at, then depth and waveform.

### Config
* `%master float` -- scale master volume
* `%mix float ...` -- scale each channel volume
* `%samples int` -- sample rate
* `%key int` -- transpose song by half-steps
* `%reverb bit ...` -- toggle reverb per channel


## Bads
* Format is pretty contrived. Had I known the kind of tricks I could pull regarding `window` in the beginning it'd be a lot cleaner, and the compiled example song would be a lot smaller than 74kB (which is surprisingly AFTER some egregious compression tricks)!
* Pulse modulation isn't implemented correctly, use at your discretion.
* Parser is incomplete (avoid trailing spaces 😬).
* Scripting the music is TEEDDDIIOOUUSSS.
* Be careful when debugging your song in 22kHz if you intend to render at 44kHz, the noise will include the higher frequencies.
* No BPM config (yet).
* No config to control reverberation (yet).
* ***This was mostly written on a per-need basis so I could finish the example song by the BotB Spring Tracks IX deadline.
