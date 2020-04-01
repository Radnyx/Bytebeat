{--
  TODO:
    Tremolo oscillator

    Triangle wave:

    const k = (t / 44100 * f % 1);
    return 4*(k <= 0.5
      ? k - 0.25
      : 0.75 - k);

    vibrato abuse allows for FM textures
     3/2 freq, 0.1 depth, sine, H2 pretty good bass
     1 freq, 1 depth, sqr, H3 good bass too

    %mix configuration


  Radnyx 2020
--}
module Bytebeat
  ( defaultConfig
  , apply
  , norm
  , effect
  , nextVol
  , nextFreq
  , Config(..)
  , Note(..)
  , Effect(..)
  , Sequence
  , State(..)
  , Waveform
  , Oscillator
  ) where

{-- Overall song configuration --}
data Config =
  Config
    { master :: Float
    , samples :: Int
    , key :: Int
    , reverb :: [Bool]
    , mix :: [Float]
    }

defaultConfig :: Config
defaultConfig =
  Config {master = 1.0, samples = 44100, key = 0, reverb = [], mix = []}

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
    , p3 :: Oscillator
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
    , waveform = Just (False, False, False)
    , harmonics = 1
    , p0 = (0, 0, "xx")
    , p1 = (2, 0.75, "sn")
    , p2 = (0, 0, "xx")
    , p3 = (0, 0, "xx")
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
effect (O (3, o)) s = s {p3 = o} -- frequency modulation
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

{-- Normalizes silent states to default --}
norm :: State -> State
norm s =
  if freq s == 0.0 || volume s == 0.0
    then defaultState
    else s

{-- Apply each step of a sequence successively (given key offset) --}
apply :: Int -> Sequence -> [State]
apply k = aux $ defaultState {offset = k}
  where
    aux s (step:sq) = s' : aux s' sq
      where
        s' = foldr effect (updateState s) step
    aux _ [] = []

