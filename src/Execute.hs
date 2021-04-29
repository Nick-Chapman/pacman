module Execute(
  Context, State, Picture(..),
  init, runForOneFrame,
  ) where

import Prelude hiding (init)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Types (
  -- code
  Code(..), Prog(..), Step(..), E(..),
  -- values
  Keys(..), XY(..),RGB(..),
  fromBits,
  )

data Context = Context
data State = State -- everything which can form a saved-state

data Picture where
  Draw :: XY Int -> RGB Int -> Picture
  Pictures :: [Picture] -> Picture

init :: Code -> IO (Context,State,Prog)
init Code{entry=prog} = do
  -- TODO: load roms, setup ram, reg map, etc here
  pure $ (Context,State,prog)

-- make no use of IO, but nice for debug
runForOneFrame :: Prog -> Context -> State -> Keys -> IO (Picture,State)
runForOneFrame prog context state keys = do
  let rs = RS { context, state, keys, screen = screen0 }
  pure $ runProg rs prog

data RS = RS
  { context :: Context
  , keys :: Keys
  , state :: State
  , screen :: Screen
  }

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
evalStep rs@RS{screen} = \case
  S_Let var oper -> undefined var oper
  S_SetReg reg e -> undefined reg e
  S_MemWrite a b -> undefined a b
  S_SetPixel xy rgb -> do
    rs { screen =
         setScreenPixel
         screen
         (fmap (fromBits . evalE rs) xy)
         (fmap (fromBits . evalE rs) rgb)
       }

evalE :: RS -> E a -> a
evalE _rs@RS{keys=Keys{pressed}} = \case
  E_KeyDown key -> Set.member key pressed
  E_Lit a -> a
  E_Reg reg -> undefined reg
  E_Var var -> undefined var
  E_Not e -> undefined e
  E_Index i e -> undefined i e

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
