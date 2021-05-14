module Top (main) where

import Control.Monad (when)
import System (System)
import System.Environment (getArgs)
import qualified EmulateWithSdl (main)
import qualified PacGraphics (tiles,screen)
import qualified SmallExamples (square,cols)
import qualified System (Conf(..),elaborate)
import qualified PacVideo_Vhdl (theVideoSystem)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Mode conf -> run conf

data Mode = Mode Conf
data Conf = Conf
  { example :: System
  , pic :: Bool -- draw the picture (i.e. run the emulator) as well as code-gen
  , accpix :: Bool  -- acculate pixels from frame to frame
  , specializeRoms :: Bool -- load roms at code-gen time to allow specialization
  }

parseArgs :: [String] -> Mode
parseArgs = do
  loop Conf
    { example = SmallExamples.cols
    , pic = True
    , accpix = False
    , specializeRoms = False -- default slow
    }
  where
    loop :: Conf -> [String] -> Mode
    loop conf = \case
      [] -> Mode conf
      "nopic":xs -> loop conf { pic = False } xs
      "accpix":xs -> loop conf { accpix = True } xs
      "slow":xs -> loop conf { specializeRoms = False } xs
      "quick":xs -> loop conf { specializeRoms = True } xs

      "cols":xs -> loop conf { example = SmallExamples.cols } xs
      "square":xs -> loop conf { example = SmallExamples.square } xs
      "tiles":xs -> loop conf { example = PacGraphics.tiles } xs
      "screen":xs -> loop conf { example = PacGraphics.screen } xs
      "vhdl":xs -> loop conf { example = PacVideo_Vhdl.theVideoSystem } xs

      xs -> error (show ("parseArgs",xs))

run :: Conf -> IO ()
run Conf{example,pic,accpix,specializeRoms} = do
  putStrLn "*rethinking emulation types*"
  code <- System.elaborate System.Conf { specializeRoms } example
  generateFile "small" code
  when pic $ EmulateWithSdl.main code accpix
  pure ()

generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let str = show a
  let nlines = length [ () | '\n' <- str ]
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp <> " (" ++ show nlines ++ " lines)"
  writeFile fp str
