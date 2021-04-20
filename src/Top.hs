
module Top (main) where

import InstructionSet (theDecodeTable)
import System.IO (stdout)
import qualified Graphics (main)
import qualified PacEmu as Pac (emulate,Conf(..))
import PacEmu (DisControl(..))

main :: IO ()
main = do
  let _ = stdout
  let _ = Graphics.main
  let _ = print theDecodeTable
  Pac.emulate conf
  pure ()

conf :: Pac.Conf
conf = Pac.Conf
  { stop = Just 7100
  , trace = Just (stdout,DisOff)
  }
