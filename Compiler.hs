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
import Template
import Text.Printf

{-- Instrument definition --}
data Instr =
  Instr
    { instrWaveform :: Maybe Waveform
    , instrHarmonics :: Int
    , instrP0 :: Oscillator
    , instrP1 :: Oscillator
    , instrP2 :: Oscillator
    , instrP3 :: Oscillator
    }
  deriving (Eq, Ord, Show)

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

type InstrIndex = M.Map Instr Int

{-- Get the instrument of a state --}
getInstr :: State -> Instr
getInstr s =
  Instr
    { instrWaveform = waveform s
    , instrHarmonics = harmonics s
    , instrP0 = p0 s
    , instrP1 = p1 s
    , instrP2 = p2 s
    , instrP3 = p3 s
    }

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

{-- Cache all the instruments in a list of states --}
cacheInstr :: [State] -> (InstrIndex, [Instr])
cacheInstr ss = (instrs, reverse table)
  where
    aux (s:ss') =
      if M.member instr m
        then (m, tbl)
        else (M.insert instr (M.size m) m, instr : tbl)
      where
        instr = getInstr s
        (m, tbl) = aux ss'
    aux [] = (M.empty, [])
    (instrs, table) = aux ss

{-- Cache volume, frequency, instrument triplets in list of states --}
vfiIndex :: InstrIndex -> [State] -> ([VFI], [Int])
vfiIndex instrs = aux M.empty
  where
    aux m (s:ss) =
      case M.lookup vfi m of
        Nothing -> (vfi : table, i : ch)
          where i = M.size m
                (table, ch) = aux (M.insert vfi i m) ss
        Just i -> (table, i : ch)
          where (table, ch) = aux m ss
      where
        vfi = getVFI instrs s
    aux _ [] = ([], [])

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
    , genOscillator (instrP3 i)
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
  , listify $ ("\n" ++) . listify . fmap genVFI <$> vfiChannels
  , listify $ ("\n" ++) . show <$> indices)
  where
    (vfiChannels, indices) = unzip index

{-- Compile sequences --}
compile :: Config -> [Sequence] -> String
compile cfg sqs = template cfg' $ generate itable vfis
    -- Compute, normalize, and cache states
  where
    states = fmap norm . apply (key cfg) <$> sqs
    (imap, itable) = cacheInstr (join states)
    vfis = vfiIndex imap <$> states
    cfg' =
      if null (mix cfg)
        then cfg {mix = replicate (length sqs) 1}
        else cfg
