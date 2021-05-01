
module Top (main) where

import Pretty (pretty)
import System.Environment (getArgs)
import qualified SmallExamples
import qualified Compile (elab)
import qualified EmulateWithSdl (main)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Again -> again

data Mode = Again

parseArgs :: [String] -> Mode
parseArgs = \case
  [] -> Again
  xs -> error (show ("parseArgs",xs))

again :: IO ()
again = do
  putStrLn "*rethinking emulation types*"
  let example =
        --SmallExamples.driveSquare
        SmallExamples.loadCols
  let code = Compile.elab example
  putStr (pretty code)
  EmulateWithSdl.main code
  pure ()
