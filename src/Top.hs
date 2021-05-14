module Top (main) where

import Control.Monad (when)
import System (System)
import System.Environment (getArgs)
import qualified EmulateWithSdl (main)
import qualified PacGraphics (tiles,screen)
import qualified SmallExamples (square,cols)
import qualified System (Conf(..),elaborate)
import qualified PacVideo_Vhdl (theVideoSystem)
import Data.Map.Strict (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Mode conf -> run conf

data Mode = Mode Conf
data Conf = Conf
  { name :: String -- name of the system
  , system :: System -- definition
  , pic :: Bool -- draw the picture? (i.e. run the emulator) as well as code-gen
  , accpix :: Bool  -- acculate pixels from frame to frame?
  , specializeRoms :: Bool -- load roms at code-gen time to allow specialization?
  }

examples :: Map String System
examples = Map.fromList
  [ ("cols"      , SmallExamples.cols)
  , ("square"    , SmallExamples.square)
  , ("tiles"     , PacGraphics.tiles)
  , ("screen"    , PacGraphics.screen)
  , ("vhdl"      , PacVideo_Vhdl.theVideoSystem)
  ]

parseArgs :: [String] -> Mode
parseArgs args = do
  let name = "cols" -- default
  let Just system = Map.lookup name examples
  loop Conf
    { name
    , system
    , pic = True
    , accpix = False
    , specializeRoms = False -- default slow
    } args
  where
    loop :: Conf -> [String] -> Mode
    loop conf = \case
      [] -> Mode conf
      "nopic":xs -> loop conf { pic = False } xs
      "accpix":xs -> loop conf { accpix = True } xs
      "slow":xs -> loop conf { specializeRoms = False } xs
      "quick":xs -> loop conf { specializeRoms = True } xs
      name:xs ->
        case Map.lookup name examples of
          Nothing -> error (show ("parseArgs",name))
          Just system -> loop conf { name, system } xs

run :: Conf -> IO ()
run Conf{name,system,pic,accpix,specializeRoms} = do
  putStrLn "*rethinking emulation types*"
  code <- System.elaborate System.Conf { specializeRoms } system
  generateFile name code
  when pic $ EmulateWithSdl.main code accpix
  pure ()

generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let str = show a
  let nlines = length [ () | '\n' <- str ]
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp <> " (" ++ show nlines ++ " lines)"
  writeFile fp str
