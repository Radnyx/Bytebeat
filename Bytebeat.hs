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
  ( compile
  , template
  , Config(..)
  , defaultConfig
  , Note(..)
  , Effect(..)
  , Sequence
  ) where

import Data.List
import qualified Data.Map as M

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

{-- TODO --}
type Oscillator = (Float, Float, String)

{-- Modify the state of the channel --}
data Effect
  = K Key -- key to play
  | A Float -- volume (amplitude)
  | W String -- waveform
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
    , waveform :: String
    , harmonics :: Int
    , p0 :: Oscillator
    , p1 :: Oscillator
    , p2 :: Oscillator
    , offset :: Int
    }
  deriving (Eq, Ord)

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
    , waveform = "nop"
    , harmonics = 1
    , p0 = (0, 0, "nop")
    , p1 = (2, 0.5, "sinwav")
    , p2 = (0, 0, "nop")
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
update :: State -> State
update s = s {volume = nextVol s, freq = nextFreq s}

{-- Compile single channel state into byte-beat code --}
instance Show State where
  show s =
    "[" ++
    intercalate "," comp ++
    ",[" ++
    osc (p0 s) ++
    "," ++
    (if harmonics s == 1
       then "[]"
       else osc (p1 s)) ++
    "," ++ osc (p2 s) ++ "]]"
    where
      osc (f, d, w) =
        if f == 0 || d == 0
          then "[]"
          else "[" ++ show f ++ "," ++ show d ++ "," ++ w ++ "]"
      v = show $ volume s
      f = show $ freq s
      nv = show $ nextVol s
      nf = show $ nextFreq s
      v' =
        if v == nv
          then v
          else "[" ++ v ++ "," ++ nv ++ "]"
      f' =
        if f == nf
          then f
          else "[" ++ f ++ "," ++ nf ++ "]"
      comp = [v', f', waveform s, show $ harmonics s]

{-- Compile Sequence into range [-1, 1] code  --}
compile :: Config -> Sequence -> String
compile cfg sq =
  "[[" ++ intercalate ", " (show <$> table) ++ "]," ++ show ch ++ "]"
    -- Apply each step successively
  where
    compute :: Sequence -> State -> [State]
    compute (step:sq) s = s' : compute sq s'
      where
        s' = foldl (flip effect) (update s) step
    compute [] _ = []
    -- Normalizes silent states
    normSilent s =
      if freq s == 0.0 || volume s == 0.0
        then defaultState
        else s
    {--
    -- Normalizes harmonic oscillator when unused
    normHarm1 s =
      if harmonics s == 1
        then s {p1 = p1 defaultState}
        else s
    -- Normalizes vibrato oscillator when unused
    normVibe s =
      if f == 0.0 || d == 0.0
         then s {p2 = p2 defaultState}
         else s
      where
        (f, d, _) = p2 s--}
    --}
    -- Cache states into lookup table
    cache :: [State] -> M.Map State Int -> ([State], [Int])
    cache (s:ss) m =
      case M.lookup s m of
        Nothing -> (s : table, i : ch)
          where i = M.size m
                (table, ch) = cache ss (M.insert s i m)
        Just i -> (table, i : ch)
          where (table, ch) = cache ss m
    cache [] _ = ([], [])
    -- Compute, normalize, and cache states
    start = defaultState {offset = key cfg}
    states = normSilent <$> compute sq start {--. normHarm1 . normVibe--}
    (table, ch) = cache states M.empty

{-- Boilerplate and mixing engine for a song --}
template :: Config -> String -> String
template cfg song =
  "128*(sample_rate => {\n\
\  const nop = () => 0.0;\n\
\  const sinwav = (t,freq, offset=0) =>\n\
\    sin(t * 6.28 / sample_rate * freq + offset);\n\
\  const sqrwav = (t, freq, offset=0, [wf=0,wd=0,ww=sinwav]) =>\n\
\    ((t / sample_rate * freq + offset) % 1 >\n\
\      wd * ww(t, wf) * 0.5 + 0.5) * 2 - 1;\n\
\  const sawwav = (t, freq) =>\n\
\    ((t / sample_rate * freq) % 1) * 2 - 1;\n\
\  const noise = (t, freq) => {\n\
\    const nf = floor((t & 262143) * freq * 400000 / sample_rate / sample_rate);\n\
\    return ((16384 * sin(nf * nf)) & 128) / 64.0 - 1;\n\
\  };\n\n\
\  const song =\n" ++
  song ++
  ";\n\
\  const reverb=" ++
  rvrb ++
  ";\n\n\
\  function harm(t, count, vol, freq, wav, [p1freq, p1depth, p1wav], p2) {\n\
\    if (freq == 0.0) return 0.0;\n\
\    let mix = 0.0;\n\
\    mix = vol * wav(t, freq, 0, p2);\n\
\    for (let i = 2; i <= count; i++) {\n\
\      vol *= 0.75;\n\
\      mix += (p1wav(t, p1freq, i * 1) * p1depth + 0.25) * vol * wav(t,freq * i, 0, p2);\n\
\    }\n\
\    return mix;\n\
\  }\n\n\
\  function generate(t, reverb) {\n\
\    if (t < 0) return 0.0;\n\
\    const time = t / sample_rate * " ++
  show 12.93 ++
  " % song[0][1].length;\n\
\    const step = floor(time);\n\
\    const vprog = time - step;\n\
\    const fprog = floor(vprog * 8) / 8;\n\
\\n\
\    let mix = 0.0;\n\
\    for (let i = 0; i < song.length; i++) {\n\
\      if (reverb !== undefined && !reverb[i]) continue;\n\
\      const data = song[i][0][song[i][1][step]];\n\
\      const [vol, freq, wav, h, p] = data;\n\
\      const [[p0freq=0, p0depth=0, p0wav=nop], p1, p2] = p;\n\
\      const v = vol.length === undefined\n\
\        ? vol\n\
\        : vol[1] * vprog + vol[0] * (1 - vprog);\n\
\      const f = freq.length === undefined\n\
\        ? freq\n\
\        : freq[1] * fprog + freq[0] * (1 - fprog);\n\
\      mix += harm(t, h, v, f + p0wav(t, p0freq) * p0depth, wav, p1, p2);\n\
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
