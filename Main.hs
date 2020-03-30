{--
  Radnyx 2020
--}

import Bytebeat
import Compiler
import Data.List
import Parser

{-- Read several tra D0.05ck files together --}
readFiles :: [String] -> IO [String]
readFiles xs = sequenceA (readFile <$> xs)

{-- Parse and concatenate multiple tracks --}
concatTracks :: [String] -> IO (Config, [Sequence])
concatTracks = aux 1 0
    -- pattern #, num channels, written tracks
  where
    aux _ n [] = return (defaultConfig, replicate n [])
    aux p n (bbt:bbts) =
      case parseBBT bbt of
        Left e -> do
          putStrLn $ "Parser error in pattern #" ++ show p ++ "."
          error $ show e
        Right (cfg, sqs) ->
          (putStrLn $ "Lengths: " ++ show lengths) >> do
            (_, rest) <- aux (p + 1) (length sqs) bbts
            return (cfg, zipWith (++) sqs' rest)
          where lengths = length <$> sqs
                longest = maximum lengths
                resize (l, sq) = sq ++ replicate (longest - l) [N]
                sqs' = resize <$> zip lengths sqs

main :: IO ()
main = do
  (cfg, sqs) <- readFiles ["song1.bbt"] >>= concatTracks
  let song = compile cfg sqs
  putStrLn song
  --writeFile "song.js" $ template cfg song
  putStrLn "Success."
