
module PacEmu (Conf(..),emulate,DisControl(..)) where

import Control.Monad (when)
import Byte (Byte)
import InstructionSet (dis1)
import Mem (Mem)
import System.IO (Handle,hPutStrLn)
import Text.Printf (printf)
import qualified Mem (init,readIO,writeIO,read)
import qualified ZEmu as Z (State,Interaction(..),interaction,programCounter)

data Conf = Conf
  { stop :: Int --frame#
  , trace :: Maybe (Handle, Int) --frame# begin trace
  , disControl :: DisControl
  }

data DisControl = DisOn | DisOff

emulate :: Conf -> IO ()
emulate Conf{stop,trace,disControl} = do
  state0 <- initState
  loop state0 Z.interaction
  where
    loop :: State -> Z.Interaction -> IO ()
    loop s@State{frames,steps,cycles,iData} = \case

      Z.Trace z i -> do
        let doStop = frames >= stop
        if doStop then print ("STOP",steps,cycles) else do
          case trace of
            Nothing -> pure ()
            Just (handle,ff) -> do
              when (frames >= ff) $
                hPutStrLn handle (traceLine disControl s z)
          loop s { steps = steps + 1 } i

      Z.ReadMem a f -> do b <- Mem.readIO (mem s) a; loop s (f b)
      Z.WriteMem a b i -> do mem <- Mem.writeIO (mem s) a b; loop s { mem } i

      Z.OutputPort port val i -> do
        --print ("OutputPort",steps,cycles,port,val)
        if port /= 0 then error "OutputPort, port!=0" else
          loop s { iData = val } i

      Z.Advance n f -> do
        let cycles' = cycles + n
        if cycles' >= cyclesPerFrame
          then do
          --print ("Advance/Interrupt",frames,steps,cycles,iData)
          let frames' = frames + 1
          putStrLn $ "frame: " ++ show frames'
          loop s { frames = frames', cycles = cycles' - cyclesPerFrame } (f (Just iData))
          else loop s { cycles = cycles' } (f Nothing)
          where
            cyclesPerFrame = 3072000 `div` 60

data State = State
  { frames :: Int
  , steps :: Int
  , cycles :: Int
  , mem :: Mem
  , iData :: Byte
  }

initState :: IO State
initState = do
  mem <- Mem.init
  pure $ State { frames = 0, steps = 0, cycles = 0, mem, iData = 0}

traceLine :: DisControl -> State -> Z.State -> String
traceLine disControl s@State{steps,cycles} z = do
    let _ = dis1
    unwords $
      [ printf "(%s) cyc: %s," (show steps) (show cycles)
      , show z
      , "(" ++ unwords [ show b | b <- take 4 (pcBytes s z) ] ++ ")"
      ] ++
      case disControl of
        DisOn -> [show (dis1 (pcBytes s z))]
        DisOff -> []

pcBytes :: State -> Z.State -> [Byte]
pcBytes State{mem} z = [ Mem.read mem a | a <- [Z.programCounter z ..] ]
