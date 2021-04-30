module Execute(
  Context, State, Picture(..),
  init, runForOneFrame,
  ) where

import Prelude hiding (init)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Types {-(
  -- code
  Code(..), Prog(..), Step(..), E(..),
  RegId(..), RegSpec(..), Reg(..),

  -- values
  Keys(..), XY(..),RGB(..), Bit(..),
  fromBits,
  )-}

data Context = Context -- TODO: roms will go here

-- everything which can form a saved-state
data State = State
  { regs :: Map RegId [Bit]
  -- TODO: rams will go here
  }

data Picture where
  Draw :: XY Int -> RGB Int -> Picture
  Pictures :: [Picture] -> Picture

init :: Code -> IO (Context,State,Prog) -- IO to load roms from file
init Code{entry=prog,regDecs} = do
  -- TODO: load roms, create ram,
  let regs = Map.fromList [ (r,zeroOf size) | (r,RegSpec {size}) <- regDecs ]
  pure $ (Context,State {regs},prog)

zeroOf :: Int -> [Bit]
zeroOf size = take size (repeat B0)

-- make no use of IO, but nice for debug
runForOneFrame :: Prog -> Context -> State -> Keys -> IO (Picture,State)
runForOneFrame prog context state keys = do
  let rs = RS { context, state, keys, screen = screen0, tmps = Map.empty }
  pure $ runProg rs prog

data RS = RS
  { context :: Context
  , keys :: Keys
  , state :: State
  , screen :: Screen
  , tmps :: Tmps
  }

type Tmps = Map TmpId [Bit]

runProg :: RS -> Prog -> (Picture,State)
runProg rs@RS{screen,state} = \case
  P_Halt -> (pictureScreen screen, state)
  P_Seq step prog -> do
    let rs' = evalStep rs step
    runProg rs' prog
  P_If cond this that -> do
    case evalE rs cond of
      True -> runProg rs this
      False -> runProg rs that

evalStep :: RS -> Step -> RS
evalStep rs@RS{screen,state=s@State{regs},tmps} = \case
  S_Let (Tmp1 tmpId) oper ->
    rs { tmps = update tmps tmpId [evalOper rs oper] }
  S_SetReg (Reg1 regId) e ->
    rs { state = s { regs = update regs regId [evalE rs e] } }
  --S_MemWrite a b -> undefined a b
  S_SetPixel xy rgb -> do
    rs { screen =
         setScreenPixel
         screen
         (fmap (fromBits . evalE rs) xy)
         (fmap (fromBits . evalE rs) rgb)
       }

evalOper :: RS -> Oper a -> a
evalOper rs = \case
  O_MemRead{} -> undefined
  O_And e1 e2 -> andBit (evalE rs e1) (evalE rs e2)

evalE :: RS -> E a -> a
evalE rs@RS{keys=Keys{pressed},state=State{regs},tmps} = \case
  E_KeyDown key -> if Set.member key pressed then B1 else B0
  E_TestBit e -> isOn (evalE rs e)
  E_Lit a -> a
  E_Reg (Reg1 regId) ->
    case (look regs regId) of
      [b] -> b
      bits -> error (show ("evalE/Reg1",regId,length bits))

  E_Tmp (Tmp1 tmpId) ->
    case (look tmps tmpId) of
      [b] -> b
      bits -> error (show ("evalE/Tmp1",tmpId,length bits))

  E_Not e -> notBit (evalE rs e)
  --E_Index i e -> undefined i e


isOn :: Bit -> Bool
isOn = \case B1 -> True; B0 -> False

notBit :: Bit -> Bit
notBit = \case B1 -> B0; B0 -> B1

andBit :: Bit -> Bit -> Bit
andBit = \case
  B0 -> \_ -> B0
  B1 -> \x -> x

look :: (Ord k, Show k) => Map k v -> k -> v
look m k = maybe (error (show ("look/missing",k))) id $ Map.lookup k m

update :: (Ord k, Show k) => Map k v -> k -> v -> Map k v
update m k v =
  {-case Map.lookup k m of
    Nothing -> error (show ("update/missing",k))
    Just{} -> -}
      Map.insert k v m

----------------------------------------------------------------------
-- Screen (canvas to collect the pixels) -- TODO: is this really needed?

data Screen = Screen { m :: Map (XY Int) (RGB Int)}

screen0 :: Screen
screen0 = Screen { m = Map.empty }

setScreenPixel :: Screen -> XY Int -> RGB Int -> Screen
setScreenPixel Screen{m} xy rgb = Screen { m = Map.insert xy rgb m }

pictureScreen :: Screen -> Picture
pictureScreen Screen{m} = Pictures [ Draw (shift xy) rgb | (xy,rgb) <- Map.toList m ]
  where
    shift XY{x,y} = XY { x = x+8, y = y+8 }