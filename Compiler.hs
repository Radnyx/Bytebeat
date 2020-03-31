{--
  Radnyx 2020
--}
module Compiler
  ( compile
  ) where

import Bytebeat
import Control.Monad
import Data.List
import qualified Data.Map as M
import Text.Printf

type InstrIndex = M.Map Instr Int

{-- Verb, Frequency, Instrument element --}
data VFI =
  VFI
    { vfiVol1 :: Float
    , vfiVol2 :: Float
    , vfiFreq1 :: Float
    , vfiFreq2 :: Float
    , vfiInstr :: Int -- index into instrument cache
    }
  deriving (Eq, Ord, Show)

{-- Get the VFI of a state --}
getVFI :: InstrIndex -> State -> VFI
getVFI instrs s =
  VFI
    { vfiVol1 = volume s
    , vfiVol2 = nextVol s
    , vfiFreq1 = freq s
    , vfiFreq2 = nextFreq s
    , vfiInstr = instrs M.! getInstr s
    }

{-- Apply each step of a sequence successively --}
apply :: State -> Sequence -> [State]
apply s (step:sq) = s' : apply s' sq
  where
    s' = foldr effect (updateState s) step
apply _ [] = []

{-- Normalizes silent states to default --}
norm :: State -> State
norm s =
  if freq s == 0.0 || volume s == 0.0
    then defaultState
    else s

{-- Cache all the instruments in a list of states --}
cacheInstr :: [State] -> (InstrIndex, [Instr])
cacheInstr (s:ss) =
  if M.member instr m
    then (m, table)
    else (M.insert instr (M.size m) m, instr : table)
  where
    instr = getInstr s
    (m, table) = cacheInstr ss
cacheInstr [] = (M.empty, [])

{-- Cache volume, frequency, instrument triplets in list of states --}
vfiIndex :: InstrIndex -> M.Map VFI Int -> [State] -> ([VFI], [Int])
vfiIndex instrs m (s:ss) =
  case M.lookup vfi m of
    Nothing -> (vfi : table, i : ch)
      where i = M.size m
            (table, ch) = vfiIndex instrs (M.insert vfi i m) ss
    Just i -> (table, i : ch)
      where (table, ch) = vfiIndex instrs m ss
  where
    vfi = getVFI instrs s
vfiIndex _ _ [] = ([], [])

{-- Truncate to 2 decimal places --}
decimal :: Float -> String
decimal = printf "%.2f"

{-- "[a,b,c,d,...]" format --}
listify :: [String] -> String
listify xs = "[" ++ intercalate "," xs ++ "]"

{-- "[freq, depth, waveform]" or "[]" if turned off --}
genOscillator :: Oscillator -> String
genOscillator (f, d, w) =
  if f == 0.0 || d == 0.0 || w == "xx"
    then "[]"
    else listify [decimal f, show d, w]

{-- [[x,y,z] or xx/ns, harmonics, p0, p1, p2] --}
genInstr :: Instr -> String
genInstr i =
  listify
    [ case instrWaveform i of
        Nothing -> "ns"
        Just (x, y, z) ->
          if not $ or waves
            then "xx"
            else listify $
                 (\b ->
                    if b
                      then "1"
                      else "0") <$>
                 waves
          where waves = [x, y, z]
    , show (instrHarmonics i)
    , genOscillator (instrP0 i)
    , genOscillator (instrP1 i)
    , genOscillator (instrP2 i)
    ]

{-- [[vol1, vol2], [freq1, freq2], instr] --}
genVFI :: VFI -> String
genVFI vfi = listify [v', f', show $ vfiInstr vfi]
  where
    v1 = decimal $ vfiVol1 vfi
    v2 = decimal $ vfiVol2 vfi
    f1 = decimal $ vfiFreq1 vfi
    f2 = decimal $ vfiFreq2 vfi
    v' =
      if v1 == v2
        then v1
        else listify [v1, v2]
    f' =
      if f1 == f2
        then f1
        else listify [f1, f2]

{-- Convert instrument table and VFI index to javascript array --}
generate :: [Instr] -> [([VFI], [Int])] -> (String, String, String)
generate instrs index =
  ( listify $ genInstr <$> instrs
  , listify $ ("\n"++) . listify . fmap genVFI <$> vfiChannels
  , listify $ ("\n"++) . show <$> indices)
  where
    (vfiChannels, indices) = unzip index

{-- Compile sequences --}
compile :: Config -> [Sequence] -> (String, String, String)
compile cfg sqs = generate (reverse itable) vfis
  where
    apply' = apply (defaultState {offset = key cfg})
    -- Compute, normalize, and cache states
    states = fmap norm . apply' <$> sqs
    (imap, itable) = cacheInstr (join states)
    vfis = vfiIndex imap M.empty <$> states
