
module Top (main) where

import System.IO (stdout)
import TraceEmu (traceEmulate,TraceConf(..))
import qualified Graphics (main)
import InstructionSet (theDecodeTable)

main :: IO ()
main = do
  let _ = Graphics.main
  let _ = print theDecodeTable
  emu
  pure ()

emu :: IO ()
emu = do
  traceEmulate stdout traceConf

traceConf :: TraceConf
traceConf = TraceConf
  { stopAfter = Just 100
  , iPeriod = 1
  }
