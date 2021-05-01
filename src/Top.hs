
module Top (main) where

import Control.Monad (when)
import System.Environment (getArgs)
import qualified Code (pretty)
import qualified EmulateWithSdl (main)
import qualified SmallExamples (combined)
import qualified System (elaborate)

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
  let code = System.elaborate example
  putStr (Code.pretty code)
  when pic $ EmulateWithSdl.main code
  pure ()
