
module TraceEmu (TraceConf(..),traceEmulate) where

import Control.Monad (when)
import Emulate (EmuState(..),initState,CB(..),emulate)
import System.IO (Handle,hPutStrLn)

data TraceConf = TraceConf
  { stopAfter :: Maybe Int
  , iPeriod :: Int
  }

traceEmulate :: Handle -> TraceConf -> IO ()
traceEmulate handle TraceConf{stopAfter,iPeriod} = do
  state <- initState
  loop state
  where
    trace :: EmuState -> IO ()
    trace s = do
      let EmuState{icount} = s
      let onPeriod = icount `mod` iPeriod == 0
      let isStop = case stopAfter of Just i -> (icount > i); Nothing -> False
      when (onPeriod && not isStop) $
        hPutStrLn handle $ show s

    loop :: EmuState -> IO ()
    loop pre = do
      post@EmuState{icount} <- emulate CB { trace } pre
      let isStop = case stopAfter of Just i -> (icount > i); Nothing -> False
      case isStop of
        True -> hPutStrLn handle "STOP"
        False -> loop post
