{--
  Radnyx 2020
--}
import Bytebeat
import Compiler
import Parser
import System.Environment
import System.Exit

{-- Parse and concatenate multiple tracks --}
concatTracks :: [String] -> IO (Config, [Sequence])
concatTracks bbts = do
  let check (p, Left e) = do
        putStrLn $ "Parser error in track #" ++ show p ++ "."
        print e
        exitWith (ExitFailure 1)
      check (_, Right x) = return x
  let resize sqs = do
        putStrLn $ "Lengths: " ++ show lengths
        return $ rsz <$> zip lengths sqs
        where
          lengths = length <$> sqs
          rsz (l, sq) = sq ++ replicate (maximum lengths - l) [N]
  -- Validate parse for each track
  (cfg:_, sqs) <-
    unzip <$> mapM check (zip [1 .. length bbts] (parseBBT <$> bbts))
  -- Resize sequences to uniform track lengths, concatenate each track together
  let emptyTrack = replicate (length $ head sqs) []
  sqs' <- foldr (zipWith (++)) emptyTrack <$> mapM resize sqs
  return (cfg, sqs')

{-- Ensure correct argument format --}
checkArgs :: [String] -> IO [String]
checkArgs args@(_:_:_) = return args
checkArgs _ = do
  putStrLn "Expected arguments: <output>.js <input1>.bbt <input2>.bbt ..."
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  (output:input) <- getArgs >>= checkArgs
  (cfg, sqs) <- mapM readFile input >>= concatTracks
  writeFile output $ compile cfg sqs
  putStrLn "Success."
