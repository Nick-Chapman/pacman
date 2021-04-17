
module TraceEmu (TraceConf(..),traceEmulate) where

--import Addr (Addr(..))
--import Buttons (buttons0)
--import Byte (Byte)
import Control.Monad (when)
--import Data.Bits (testBit)
import Emulate (EmuState(..),initState,CB(..),emulate,programCounter)
--import InstructionSet (Instruction) --,prettyInstructionBytes)
--import Mem (Mem)
import System.IO (Handle,hPutStrLn)
import Text.Printf (printf)
import qualified Mem (read)
--import qualified Rom (loadInvaders)

data TraceConf = TraceConf
  { stopAfter :: Maybe Int
  , iPeriod :: Int
  , showPixs :: Bool
  }

traceEmulate :: Handle -> TraceConf -> IO ()
traceEmulate handle TraceConf{stopAfter,iPeriod,showPixs=_} = do
  state <- initState
  loop state
  where
{-    traceI :: EmuState -> Instruction Byte -> IO ()
    traceI s instruction = do
      let EmuState{icount} = s
      let onPeriod = icount `mod` iPeriod == 0
      let isStop = case stopAfter of Just i -> (icount > i); Nothing -> False
      when (onPeriod && not isStop) $
        hPutStrLn handle $ traceLineI showPixs s instruction
-}

    traceState :: EmuState -> IO ()
    traceState s = do
      let EmuState{icount} = s
      let onPeriod = icount `mod` iPeriod == 0
      let isStop = case stopAfter of Just i -> (icount > i); Nothing -> False
      when (onPeriod && not isStop) $
        hPutStrLn handle $ traceLine s


    cb = CB { traceState } --, traceI = Just traceI }

    loop :: EmuState -> IO ()
    loop pre = do
      post@EmuState{icount} <- emulate cb buttons0 pre
      let isStop = case stopAfter of Just i -> (icount > i); Nothing -> False
      case isStop of
        True -> hPutStrLn handle "STOP"
        False -> loop post

    buttons0 = ()

traceLine :: EmuState -> String -- TODO: make this show of EmuState
traceLine s@EmuState{ticks,icount,mem} = do
  unwords
    [ show s
    , "(" ++ unwords [ show b | a <- take 4 [programCounter s ..], let b = Mem.read mem a ] ++ "),"
    , printf "cyc: %s," (show ticks)
    , printf "(%d)" icount
    ]


{-traceLineI :: Bool -> EmuState -> Instruction Byte -> String
traceLineI showPixs s@EmuState{ticks,icount,mem} i = do
  let pixs = onPixels (getDisplayFromMem mem)
  unwords
    [ printf "%8d" icount
    , rjust 11 (show ticks)
    , show s
    , ":"
    --, ljust 10 (prettyInstructionBytes i)
    , if showPixs
      then unwords [ ljust 15 (show i), printf "#pixs:%d" (length pixs) ]
      else show i
    ]
-}

{-
data OnPixel = OnPixel { x :: Int, y :: Int }

data Display = Display { onPixels :: [OnPixel] }

getDisplayFromMem :: Mem -> Display
getDisplayFromMem mem = do
  Display
    [ OnPixel {x, y}
    | x :: Int <- [0..223]
    , yByte <- [0..31]
    , let byte = Mem.read mem (Addr (fromIntegral (0x2400 + x * 32 + yByte)))
    , yBit <- [0..7]
    , byte `testBit` yBit
    , let y  = 8 * yByte + yBit
    ]
-}

{-
ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

rjust :: Int -> String -> String
rjust n s = take (max 0 (n - length s)) (repeat ' ') <> s
-}
