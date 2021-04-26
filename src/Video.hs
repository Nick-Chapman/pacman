
module Video (
  Eff(..),Phase(..),
  compile, Prog,
  run, Picture(..),XY(..),RGB(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Word8 (Word8)
import qualified Data.Map.Strict as Map

----------------------------------------------------------------------
-- Effects

class Phase p where
  type Number p
  type Byte p

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind

data Eff p a where
  Ret :: a -> Eff p a
  Bind :: Eff p a -> (a -> Eff p b) -> Eff p b
  LitI :: Int -> Eff p (Number p)
  LitB :: Word8 -> Eff p (Byte p)
  SetPixel :: Number p -> Number p -> RGB (Byte p) -> Eff p ()
  Add :: Number p -> Number p -> Eff p (Number p)
  Mul :: Number p -> Number p -> Eff p (Number p)

----------------------------------------------------------------------
-- compile Effect to Program

data CompileTime
instance Phase CompileTime where
  type Number CompileTime = AtomInt
  type Byte CompileTime = AtomByte

compile :: Eff CompileTime () -> Prog
compile eff0 = do
  let
    loop :: CS -> Eff CompileTime a -> (CS -> a -> Prog) -> Prog
    loop s eff1 k = case eff1 of
      Ret a -> k s a
      Bind e f -> loop s e (\s a -> loop s (f a) k)
      LitI i -> k s (A_LitI i)
      LitB b -> k s (A_LitB b)
      SetPixel x y col -> P_SetPixel x y col (k s ())
      Mul a1 a2 -> do compileFormI s (MulI a1 a2) k
      Add a1 a2 -> do compileFormI s (AddI a1 a2) k

  loop CompileState { u = 1 } eff0 (\_ () -> P_Halt)

compileFormI :: CS -> FormI AtomInt -> (CS -> AtomInt -> Prog) -> Prog
compileFormI s form k = do
  case tryConstFold form of
    Just cf -> k s (A_LitI (evFormI cf))
    Nothing -> do
      genSym s $ \s sym -> do -- TODO: smart share, only when not atomic
        P_Let sym form $ -- TODO: smart addI, fold constanst
          k s (A_SymI sym)

genSym :: CS -> (CS -> Sym -> r) -> r
genSym s@CompileState{u} f = f s { u = 1 + u } (Sym $ "u" ++ show u)

data CS = CompileState { u :: Int }

----------------------------------------------------------------------
-- Programs

data Prog
  = P_Halt
  | P_SetPixel AtomInt AtomInt (RGB AtomByte) Prog
  | P_Let Sym (FormI AtomInt) Prog
  deriving Show

data AtomInt
  = A_LitI Int
  | A_SymI Sym
  deriving Show

data AtomByte
  = A_LitB Word8
--  | A_SymB Sym
  deriving Show

newtype Sym = Sym { unSym :: String } deriving (Eq,Ord,Show)

----------------------------------------------------------------------
-- expression forms
-- constant forms can be evaluated at compile time
-- symbolic will remain until runtime

data FormI a
  = AddI a a
  | MulI a a
  deriving (Show, Functor)

evFormI :: FormI Int -> Int
evFormI = \case
  AddI x y -> x+y
  MulI x y -> x*y

-- TODO: clever/extensible way to code this
tryConstFold :: FormI AtomInt -> Maybe (FormI Int)
tryConstFold = \case
  AddI (A_LitI x) (A_LitI y) -> Just (AddI x y)
  MulI (A_LitI x) (A_LitI y) -> Just (MulI x y)
  _ -> Nothing

----------------------------------------------------------------------
-- run a Program; generate a Picture

run :: Prog -> Picture
run = do
  let
    evA :: AtomByte -> Word8
    evA = \case
      A_LitB x -> x
  let
    evI :: RS -> AtomInt -> Int
    evI RuntimeState{mi}= \case
      A_LitI x -> x
      A_SymI sym -> look sym mi

    evRGB :: RGB AtomByte -> RGB Word8
    evRGB = fmap evA
  let
    loop :: RS -> Prog -> RS
    loop s = \case
      P_Halt -> s
      P_SetPixel x y rgb prog -> do
        loop s
          { screen =
            setScreenPixel (screen s) (XY (evI s x) (evI s y)) (evRGB rgb)
          } prog
      P_Let sym form prog -> do
        let val :: Int = evFormI (fmap (evI s) form)
        loop s { mi = Map.insert sym val (mi s) } prog

  pictureScreen . screen . loop s0

data RS = RuntimeState
  { screen :: Screen
  , mi :: Map Sym Int
  }

s0 :: RS
s0 = RuntimeState { screen = screen0, mi = Map.empty }

look :: (Show k,Ord k) => k -> Map k v -> v
look k m = maybe (error (show ("look:no-value!",k))) id $ Map.lookup k m

----------------------------------------------------------------------
-- Screen (canvas to collect the pixles) -- TODO: is this really needed?

data Screen = Screen { m :: Map XY (RGB Word8)}

screen0 :: Screen
screen0 = Screen { m = Map.empty }

setScreenPixel :: Screen -> XY -> RGB Word8 -> Screen
setScreenPixel Screen{m} xy rgb = Screen { m = Map.insert xy rgb m }

pictureScreen :: Screen -> Picture
pictureScreen Screen{m} = Pictures [ Draw (shift xy) rgb | (xy,rgb) <- Map.toList m ]
  where
    shift XY{x,y} = XY { x = x+8, y = y+8 }

----------------------------------------------------------------------

data Picture where
  Draw :: XY -> RGB Word8 -> Picture
  Pictures :: [Picture] -> Picture

data XY = XY { x :: Int, y :: Int } deriving (Eq,Ord,Show)
data RGB a = RGB { r :: a, g :: a, b :: a } deriving (Show,Functor)
