
module Top (main) where

import InstructionSet (theDecodeTable)
import System.IO (stdout)
import TraceEmu (traceEmulate,TraceConf(..))
import qualified Graphics (main)

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
  { stopAfter = Nothing --Just 1000
  , iPeriod = 1
  }
