
module Top (main) where

import Control.Monad (when)
import System.Environment (getArgs)
import qualified EmulateWithSdl (main)
import qualified SmallExamples (combined)
import qualified System (Conf(..),elaborate)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Again pic -> again pic

data Mode = Again Bool

parseArgs :: [String] -> Mode
parseArgs = \case
  [] -> Again False
  ["pic"] -> Again True
  xs -> error (show ("parseArgs",xs))

again :: Bool -> IO ()
again pic = do
  putStrLn "*rethinking emulation types*"
  let example = SmallExamples.combined
  let specializeRoms = False
  code <- System.elaborate System.Conf { specializeRoms } example
  generateFile "small" code
  when pic $ EmulateWithSdl.main code
  pure ()

generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show a)
