{--
  Radnyx 2020
--}

import Bytebeat
import Compiler
import Parser

{-- Read several tra D0.05ck files together --}
readFiles :: [String] -> IO [String]
readFiles xs = sequenceA (readFile <$> xs)

{-- Parse and concatenate multiple tracks --}
concatTracks :: [String] -> IO (Config, [Sequence])
concatTracks = aux 1 0
    -- pattern #, num channels, written tracks
  where
    aux :: Int -> Int -> [String] -> IO (Config, [Sequence])
    aux _ n [] = return (defaultConfig, replicate n [])
    aux p _ (bbt:bbts) =
      case parseBBT bbt of
        Left e -> do
          putStrLn $ "Parser error in pattern #" ++ show p ++ "."
          error $ show e
        Right (cfg, sqs) ->
          putStrLn ("Lengths: " ++ show lengths) >>
            aux (p + 1) (length sqs) bbts >>= \(_, rest) ->
              return (cfg, zipWith (++) sqs' rest)
          where lengths = length <$> sqs
                longest = maximum lengths
                resize (l, sq) = sq ++ replicate (longest - l) [N]
                sqs' = resize <$> zip lengths sqs

main :: IO ()
main = do
  (cfg, sqs) <- readFiles [ "test.bbt"] >>= concatTracks
  writeFile "song.js" $ compile cfg sqs
  putStrLn "Success."
