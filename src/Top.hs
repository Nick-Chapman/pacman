
module Top (main) where

import System.Environment (getArgs)
import qualified AnExampleSystem (small)
import qualified Elaborate (elab)
import qualified EmulateWithSdl (main)
import Pretty (pretty)

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
  let code = Elaborate.elab AnExampleSystem.small
  putStr (pretty code)
  let _ = EmulateWithSdl.main code
  pure ()
