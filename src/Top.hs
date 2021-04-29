
module Top (main) where

import System.Environment (getArgs)

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
