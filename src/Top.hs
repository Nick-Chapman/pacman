
module Top (main) where

import System.IO (stdout)
import TraceEmu (traceEmulate,TraceConf(..))
import qualified Graphics (main)

main :: IO ()
main = do
  let _ = Graphics.main
  emu

emu :: IO ()
emu = do
  traceEmulate stdout traceConf

traceConf :: TraceConf
traceConf = TraceConf
  { stopAfter = Just 100
  , iPeriod = 1
  , showPixs = False
  }
