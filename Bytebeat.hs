{--
  TODO:
    Tremolo oscillator

    Triangle wave:

    const k = (t / 44100 * f % 1);
    return 4*(k <= 0.5
      ? k - 0.25
      : 0.75 - k);

    compressing data is VITAL !

     rename wave functions to one letter
     X, S, Q, W, N

     another level of indirection to instruments?
     often the only things that change are volume/freq
     so instruments built from (waveform, harm, oscillators)





  Radnyx 2020
--}
module Bytebeat
  ( template
  , defaultConfig
  , defaultState
  , updateState
  , getInstr
  , effect
  , nextVol
  , nextFreq
  , Config(..)
  , Note(..)
  , Effect(..)
  , Sequence
  , State(..)
  , Instr(..)
  , Oscillator
  ) where

import Data.List

{-- Overall song configuration --}
data Config =
  Config
    { master :: Float
    , samples :: Int
    , key :: Int
    , reverb :: [Bool]
    }

defaultConfig :: Config
defaultConfig = Config {master = 1.0, samples = 44100, key = 0, reverb = []}

{-- Chromatic scale degrees  --}
data Note
  = C_
  | Cs
  | Db
  | D_
  | Ds
  | Eb
  | E_
  | F_
  | Fs
  | Gb
  | G_
  | Gs
  | Ab
  | A_
  | As
  | Bb
  | B_

chrom :: Note -> Int
chrom note =
  case note of
    C_ -> 0
    Cs -> 1
    Db -> 1
    D_ -> 2
    Ds -> 3
    Eb -> 3
    E_ -> 4
    F_ -> 5
    Fs -> 6
    Gb -> 6
    G_ -> 7
    Gs -> 8
    Ab -> 8
    A_ -> 9
    As -> 10
    Bb -> 10
    B_ -> 11

{-- (Chromatic degree, Octave) --}
type Key = (Note, Int)

{-- Toggle sin/sqr/saw waveforms to mix --}
type Waveform = (Bool, Bool, Bool)

{-- Frequency, depth, waveforms --}
type Oscillator = (Float, Float, String)

{-- Modify the state of the channel --}
data Effect
  = K Key -- key to play
  | A Float -- volume (amplitude)
  | W (Maybe Waveform) -- waveform
  | D Float -- dampen volume per step
  | S Float -- slide frequency per step
  | O (Int, Oscillator) -- (param #, (freq, depth, wav))
  | H Int -- number of harmonics
  | X -- cut note
  | N -- no-op

{-- Tracker sequence --}
type Sequence = [[Effect]]

{-- Keeps the state of a channel in a sequence --}
data State =
  State
    { volume :: Float
    , dampen :: Float
    , freq :: Float
    , slide :: Float
    , waveform :: Maybe Waveform -- Nothing = noise
    , harmonics :: Int
    , p0 :: Oscillator
    , p1 :: Oscillator
    , p2 :: Oscillator
    , offset :: Int
    }
  deriving (Eq, Ord)

{-- Instrument definition --}
data Instr =
  Instr
    { instrWaveform :: Maybe Waveform
    , instrHarmonics :: Int
    , instrP0 :: Oscillator
    , instrP1 :: Oscillator
    , instrP2 :: Oscillator
    }
  deriving (Eq, Ord, Show)

{-- Get the instrument of a state --}
getInstr :: State -> Instr
getInstr s =
  Instr
    { instrWaveform = waveform s
    , instrHarmonics = harmonics s
    , instrP0 = p0 s
    , instrP1 = p1 s
    , instrP2 = p2 s
    }

{-- Compute the frequency of a given key --}
keyFreq :: Int -> Key -> Float
keyFreq off (note, octave) = 440.0 * 2.0 ** ((n - 49) / 12.0)
  where
    n = fromIntegral (off + 4 + chrom note + octave * 12)

{-- Channels will be initialized to this --}
defaultState :: State
defaultState =
  State
    { volume = 0.0
    , dampen = 0.0
    , freq = 0.0
    , slide = 0.0
    , waveform = Just (False, False, False)
    , harmonics = 1
    , p0 = (0, 0, "no")
    , p1 = (2, 0.75, "sn")
    , p2 = (0, 0, "no")
    , offset = 0
    }

{-- Apply a single effect to a state --}
effect :: Effect -> State -> State
effect (K k) s = s {freq = keyFreq (offset s) k}
effect (A a) s = s {volume = a}
effect (W w) s = s {waveform = w}
effect (D d) s = s {dampen = d}
effect (S f) s = s {slide = f}
effect (H h) s = s {harmonics = h}
effect X s = s {freq = 0.0}
effect (O (0, o)) s = s {p0 = o} -- vibrato
effect (O (1, o)) s = s {p1 = o} -- harmonics
effect (O (2, o)) s = s {p2 = o} -- pulse w./other
effect (O (i, _)) _ = error $ "Undefined oscillator: " ++ show i
effect N s = s

{-- Compute the next dampened volume --}
nextVol :: State -> Float
nextVol s = max (volume s - dampen s) 0

{-- Compute next frequency slide --}
nextFreq :: State -> Float
nextFreq s =
  if f > 20000
    then 0
    else f
  where
    f = max (freq s + slide s) 0

{-- Update a state given its parameters --}
updateState :: State -> State
updateState s = s {volume = nextVol s, freq = nextFreq s}

{-- Boilerplate and mixing engine for a song --}
template :: Config -> String -> String
template cfg song =
  "128*(sample_rate => {\n\
\  const xx = () => 0.0;\n\
\  const sn = (t,freq, offset=0) =>\n\
\    sin(t * 6.28 / sample_rate * freq + offset);\n\
\  const sq = (t, freq, offset=0, [wf=0,wd=0,ww=sn]) =>\n\
\    ((t / sample_rate * freq + offset) % 1 >\n\
\      wd * ww(t, wf) * 0.5 + 0.5) * 2 - 1;\n\
\  const sw = (t, freq) =>\n\
\    ((t / sample_rate * freq) % 1) * 2 - 1;\n\
\  const ns = (t, freq) => {\n\
\    const nf = floor((t & 262143) * freq * 400000 / sample_rate / sample_rate);\n\
\    return ((16384 * sin(nf * nf)) & 128) / 64.0 - 1;\n\
\  };\n\n\
\  const song =\n" ++
  song ++
  ";\n\
\  const reverb=" ++
  rvrb ++
  ";\n\n\
\  function layer(t, freq, waves, p2) {\n\
\    if (waves.concat === undefined) return waves(t, freq);\n\
\    let mix = 0.0;\n\
\    if (waves[0]) mix += sn(t, freq) * 1.0;\n\
\    if (waves[1]) mix += sq(t, freq, 0, p2) * 0.8;\n\
\    if (waves[2]) mix += sw(t, freq) * 0.8;\n\
\    return mix;\n\
\  }\n\n\
\  function harm(t, count, vol, freq, waves, [p1freq, p1depth, p1wav], p2) {\n\
\    if (freq == 0.0) return 0.0;\n\
\    let mix = 0.0;\n\
\    mix = vol * layer(t, freq, waves, p2);\n\
\    for (let i = 2; i <= count; i++) {\n\
\      vol *= 0.75;\n\
\      mix += (p1wav(t, p1freq, i) * p1depth) * vol * layer(t, freq * i, waves, p2);\n\
\    }\n\
\    return mix;\n\
\  }\n\n\
\  function generate(t, reverb) {\n\
\    if (t < 0) return 0.0;\n\
\    const time = t / sample_rate * " ++
  show 12.93 ++
  " % song[1][0][1].length;\n\
\    const step = floor(time);\n\
\    const vprog = time - step;\n\
\    const fprog = floor(vprog * 8) / 8;\n\
\\n\
\    let mix = 0.0;\n\
\    for (let i = 0; i < song[1].length; i++) {\n\
\      if (reverb !== undefined && !reverb[i]) continue;\n\
\      const vfi = song[1][i][1][step];\n\
\      const [vol, freq, instr] = song[1][i][0][vfi];\n\
\      const [waves, h, [p0freq=0, p0depth=0, p0wav=xx], p1, p2] = song[0][instr];\n\
\      const v = vol.concat === undefined\n\
\        ? vol\n\
\        : vol[1] * vprog + vol[0] * (1 - vprog);\n\
\      const f = freq.concat === undefined\n\
\        ? freq\n\
\        : freq[1] * fprog + freq[0] * (1 - fprog);\n\
\      mix += harm(t, h, v, f + p0wav(t, p0freq) * p0depth, waves, p1, p2);\n\
\    }\n\
\    return mix;\n\
\  }\n\
\\n\
\  return (generate(t) + 0.25 * generate(t - sample_rate * 0.2, reverb)) * " ++
  show (master cfg) ++
  ";\n\
\\n\
\})(" ++
  show (samples cfg) ++ ") + 127"
  where
    rvrb =
      "[" ++
      intercalate
        ", "
        ((\x ->
            if x
              then "true"
              else "false") <$>
         reverb cfg) ++
      "]"
