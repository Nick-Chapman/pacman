
module PacEmu (Conf(..),emulate,DisControl(..)) where

import Byte (Byte)
import InstructionSet (dis1)
import Mem (Mem)
import System.IO (Handle,hPutStrLn)
import Text.Printf (printf)
import qualified Mem (init,readIO,writeIO,read)
import qualified ZEmu as Z (State,Interaction(..),interaction,programCounter)

data Conf = Conf
  { stop :: Maybe Int
  , trace :: Maybe (Handle,DisControl)
  }

data DisControl = DisOn | DisOff

emulate :: Conf -> IO ()
emulate Conf{stop,trace} = do
  state0 <- initState
  loop state0 Z.interaction
  where
    loop :: State -> Z.Interaction -> IO ()
    loop s@State{steps,cycles} = \case

      Z.Trace z i -> do
        case trace of
          Nothing -> pure ()
          Just (handle,disControl) -> do
            hPutStrLn handle (traceLine disControl s z)
        loop s { steps = steps + 1 } i

      Z.ReadMem a f -> do b <- Mem.readIO (mem s) a; loop s (f b)
      Z.WriteMem a b i -> do mem <- Mem.writeIO (mem s) a b; loop s { mem } i

      Z.OutputPort _port _val i -> do
        print ("OutputPort",steps,cycles,_port,_val) -- TODO
        loop s i

      Z.Advance n f -> do
        let
          cycles' = cycles + n
          doStop = case stop of Just i -> (steps > i); Nothing -> False
        if doStop then print ("STOP",steps,cycles) else
          loop s { cycles = cycles' } (f Nothing)

data State = State
  { steps :: Int
  , cycles :: Int
  , mem :: Mem
  }

initState :: IO State
initState = do
  mem <- Mem.init
  pure $ State { steps = 0, cycles = 0, mem}

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
