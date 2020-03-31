{--
  Tracker DSL

  Radnyx 2020
--}
module Parser
  ( parseBBT
  ) where

import Bytebeat
import Data.Char
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

{-- Convert  --}
note :: Char -> Char -> Note
note 'C' '-' = C_
note 'C' '#' = Cs
note 'D' 'b' = Db
note 'D' '-' = D_
note 'D' '#' = Ds
note 'E' 'b' = Eb
note 'E' '-' = E_
note 'F' '-' = F_
note 'F' '#' = Fs
note 'G' 'b' = Gb
note 'G' '-' = G_
note 'G' '#' = Gs
note 'A' 'b' = Ab
note 'A' '-' = A_
note 'A' '#' = As
note 'B' 'b' = Bb
note 'B' '-' = B_
note _ _ = error "Unexpected symbols when parsing note."

{-- Skips spaces and tabs (not newlines!) --}
skipwhite :: Parser ()
skipwhite = skipMany $ oneOf " \t"

{-- Expects then skips spaces and tabs (not newlines!) --}
skipwhite1 :: Parser ()
skipwhite1 = skipMany1 $ oneOf " \t"

{-- Matches newline or EOF --}
eol :: Parser ()
eol = void newline <|> eof

{-- Comments of form # .... \n --}
skipComment :: Parser ()
skipComment = do
  _ <- char '#'
  skipMany $ noneOf "\n"
  eol

{-- Ignore spaces and comments --}
ignore :: Parser ()
ignore = skipMany (skipComment <|> void space)

{-- Parse a binary digit 1 or 0 --}
bit :: Parser Char
bit = char '1' <|> char '0'

{-- Waveform effects --}
parseWaveName :: Parser String
parseWaveName = choice $ try . string <$> ["sn", "sq", "sw", "ns", "xx"]

{-- W100, W 0 0 1, etc. --}
parseWaveform :: Parser Effect
parseWaveform = do
  _ <- char 'W'
  skipwhite
  i <- bit
  skipwhite
  j <- bit
  skipwhite
  k <- bit
  return $ W $ Just (i == '1', j == '1', k == '1')

{-- E-1. C#3, etc. --}
parseKey :: Parser Effect
parseKey = do
  n <- oneOf "ABCDEFG"
  acc <- oneOf "-#b"
  oct <- oneOf "012345678"
  return $ K (note n acc, ord oct - 48)

{-- "E-1" or "---" --}
parseNote :: Parser Effect
parseNote = parseKey <|> (string "---" >> return X) <|> (oneOf "=." >> return N)

{-- A 0.3, A0.3 --}
parseVolume :: Parser Effect
parseVolume = do
  _ <- char 'A'
  skipwhite
  A <$> floating

{-- Pitch slide --}
parseSlide :: Parser Effect
parseSlide = do
  _ <- char 'S'
  skipwhite
  s <- (char '-' >> floating >>= \f -> return $ -f) <|> floating
  return $ S s

{-- Number of harmonics --}
parseHarmonics :: Parser Effect
parseHarmonics = do
  _ <- char 'H'
  skipwhite
  H <$> nat

{-- V freq depth wav --}
parseVibrato :: Parser Effect
parseVibrato = do
  _ <- char 'V'
  skipwhite
  f <- floating
  skipwhite
  d <- floating
  skipwhite
  s <- parseWaveName
  return $ O (0, (f, d, s))

{-- P freq depth wav --}
parsePulse :: Parser Effect
parsePulse = do
  _ <- char 'P'
  skipwhite
  f <- floating
  skipwhite
  d <- floating
  skipwhite
  s <- parseWaveName
  return $ O (2, (f, d, s))

{-- Decrease volume per step --}
parseDampen :: Parser Effect
parseDampen = do
  _ <- char 'D'
  skipwhite
  D <$> floating

{-- noise is an alternative to waveform layering--}
parseNoise :: Parser Effect
parseNoise = do
  _ <- try $ string "noise"
  return $ W Nothing

{-- Parse any other effect than N and X --}
parseEffect :: Parser Effect
parseEffect =
  choice
    [parseVolume, parseSlide, parseHarmonics, parseVibrato,
    parseDampen, parsePulse, parseWaveform, parseNoise]

{-- Parse a single step of a sequence --}
parseStep :: Parser [Effect]
parseStep = do
  e <- parseNote
  es <- many $ skipwhite1 >> parseEffect
  eol
  return $ e : es

{-- %master float --}
parseMaster :: Config -> Parser Config
parseMaster cfg = do
  _ <- try $ string "%master"
  skipwhite
  v <- floating
  return $ cfg {master = v}

{-- %samples nat --}
parseSamples :: Config -> Parser Config
parseSamples cfg = do
  _ <- try $ string "%samples"
  skipwhite
  s <- nat
  return $ cfg {samples = s}

{-- %key int --}
parseOffset :: Config -> Parser Config
parseOffset cfg = do
  _ <- try $ string "%key"
  skipwhite
  k <- int
  return $ cfg {key = k}

{-- %reverb 1 0 ... --}
parseReverb :: Config -> Parser Config
parseReverb cfg = do
  _ <- try $ string "%reverb"
  r <- many1 $ skipwhite1 >> bit
  return $ cfg { reverb = (== '1') <$> r }

{-- Parse configuration settings at top of file --}
parseConfig :: Config -> Parser Config
parseConfig cfg = do
  ignore
  let options = [parseMaster, parseSamples, parseOffset, parseReverb]
  (choice (($ cfg) <$> options) >>= parseConfig) <|> return cfg

{-- Parse groups of sequence definitions --}
parseSequences :: Parser (Config, [Sequence])
parseSequences = do
  cfg <- parseConfig defaultConfig
  ignore
  sqs <- many $ many1 parseStep >>= \sq -> ignore >> return sq
  eol
  return (cfg, sqs)

parseBBT :: String -> Either ParseError (Config, [Sequence])
parseBBT = parse parseSequences ""
