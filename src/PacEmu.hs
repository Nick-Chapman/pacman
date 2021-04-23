
module PacEmu (
  init,State (mem),
  Conf(..), DisControl(..),emulateOneFrame,
  ) where

import Byte (Byte)
import InstructionSet (dis1)
import Mem (Mem)
import Prelude hiding (init)
import System.IO (Handle,hPutStrLn)
import Text.Printf (printf)
import qualified Mem (readIO,writeIO,read)
import qualified ZEmu as Z (State,Interaction(..),interaction,programCounter)

data State = State
  { steps :: Int
  , cycles :: Int
  , iData :: Byte
  , mem :: Mem
  , interaction :: Z.Interaction
  }

init :: Mem -> State
init mem = do
  State
    { steps = 0
    , cycles = 0
    , iData = 0
    , mem
    , interaction = Z.interaction
    }

data Conf = Conf { trace :: Maybe (Handle, DisControl) }
data DisControl = DisOn | DisOff

emulateOneFrame :: Conf -> State -> IO State
emulateOneFrame Conf{trace} state0 = do
  loop state0
  where
    loop :: State -> IO State
    loop s@State{steps,cycles,iData,interaction} = case interaction of

      Z.Trace z interaction -> do
        case trace of
          Nothing -> pure ()
          Just (handle,disControl) -> do
            hPutStrLn handle (traceLine disControl s z)
        loop s { steps = steps + 1, interaction }

      Z.ReadMem a f -> do b <- Mem.readIO (mem s) a; loop s { interaction = f b }
      Z.WriteMem a b interaction -> do
        mem <- Mem.writeIO (mem s) a b; loop s { mem, interaction }

      Z.OutputPort port val interaction -> do
        --print ("OutputPort",steps,cycles,port,val)
        if port /= 0 then error "OutputPort, port!=0" else
          loop s { iData = val, interaction }

      Z.Advance n f -> do
        case cycles + n >= cyclesPerFrame of
          False ->
            -- keep looping
            loop s { cycles = cycles + n, interaction = f Nothing }
          True -> do
            -- end loop
            pure s { cycles = cycles + n - cyclesPerFrame , interaction = f (Just iData)}

      where
        cyclesPerFrame = 3072000 `div` 60


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
