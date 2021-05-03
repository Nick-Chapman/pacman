module Top (main) where

import Control.Monad (when)
import System (System)
import System.Environment (getArgs)
import qualified EmulateWithSdl (main)
import qualified PacGraphics (tiles,sprites,screen)
import qualified SmallExamples (combined)
import qualified System (Conf(..),elaborate)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Mode conf -> run conf

data Mode = Mode Conf
data Conf = Conf { example :: System, pic :: Bool , specializeRoms :: Bool }

parseArgs :: [String] -> Mode
parseArgs = do
  loop Conf
    { example = PacGraphics.sprites
    , pic = True
    , specializeRoms = False -- default slow
    }
  where
    loop :: Conf -> [String] -> Mode
    loop conf = \case
      [] -> Mode conf
      "nopic":xs -> loop conf { pic = False } xs
      "slow":xs -> loop conf { specializeRoms = False } xs
      "quick":xs -> loop conf { specializeRoms = True } xs
      "combined":xs -> loop conf { example = SmallExamples.combined } xs
      "tiles":xs -> loop conf { example = PacGraphics.tiles } xs
      "sprites":xs -> loop conf { example = PacGraphics.sprites } xs
      "screen":xs -> loop conf { example = PacGraphics.screen } xs -- TODO
      xs -> error (show ("parseArgs",xs))

run :: Conf -> IO ()
run Conf{example,pic,specializeRoms} = do
  putStrLn "*rethinking emulation types*"
  code <- System.elaborate System.Conf { specializeRoms } example
  generateFile "small" code
  when pic $ EmulateWithSdl.main code
  pure ()

generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let str = show a
  let nlines = length [ () | '\n' <- str ]
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp <> " (" ++ show nlines ++ " lines)"
  writeFile fp str
