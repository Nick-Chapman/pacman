
module Top (main) where

import Control.Monad (when)
import Pretty (pretty)
import System.Environment (getArgs)
import qualified SmallExamples
import qualified Compile (elab)
import qualified EmulateWithSdl (main)

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
  let code = Compile.elab example
  putStr (pretty code)
  when pic $ EmulateWithSdl.main code
  pure ()
