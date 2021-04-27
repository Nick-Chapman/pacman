
module Video (
  Eff(..),Phase(..),
  compile, Prog,
  run, Picture(..),XY(..),RGB(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Bits
import Data.Map (Map)
import Machine (Machine(..))
import qualified Data.Map.Strict as Map
import qualified Rom

----------------------------------------------------------------------
-- Effects

class Phase p where
  type Number p -- model as a list of Bits?
  type Bit p

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind

data Eff p a where
  Ret :: a -> Eff p a
  Bind :: Eff p a -> (a -> Eff p b) -> Eff p b
  LitI :: Int -> Eff p (Number p)
  SetPixel :: Number p -> Number p -> RGB (Number p) -> Eff p ()
  Add :: Number p -> Number p -> Eff p (Number p)
  Mul :: Number p -> Number p -> Eff p (Number p)
  PickBit :: Number p -> Int -> Eff p (Bit p)
  CaseBit :: Bit p -> Eff p (Bool)
  ReadColRom :: Number p -> Eff p (Number p)

----------------------------------------------------------------------
-- compile Effect to Program

data CompileTime
instance Phase CompileTime where
  type Number CompileTime = AtomInt
  type Bit CompileTime = AtomBit

compile :: Eff CompileTime () -> Prog
compile eff0 = do
  let
    loop :: CS -> Eff CompileTime a -> (CS -> a -> Prog) -> Prog
    loop s eff1 k = case eff1 of
      Ret a -> k s a
      Bind e f -> loop s e (\s a -> loop s (f a) k)
      LitI i -> k s (A_LitI i)
      SetPixel x y col -> P_SetPixel x y col (k s ())
      Mul a1 a2 -> do compileFormI s (MulI a1 a2) k
      Add a1 a2 -> do compileFormI s (AddI a1 a2) k
      PickBit a i -> k s (A_Picked i a)
      CaseBit a -> do P_If a (k s True) (k s False)

      ReadColRom a -> do
        genSym s $ \s sym -> do
          P_Let sym (R_ReadColRom a) $
            k s (A_SymI sym)

  loop CompileState { u = 1 } eff0 (\_ () -> P_Halt)

compileFormI :: CS -> FormI AtomInt -> (CS -> AtomInt -> Prog) -> Prog
compileFormI s form k = do
  case tryConstFold form of
    Just cf -> k s (A_LitI (evFormI cf))
    Nothing -> do
      genSym s $ \s sym -> do -- TODO: smart share, only when not atomic
        P_Let sym (R_Form form) $ -- TODO: smart addI, fold constanst
          k s (A_SymI sym)

genSym :: CS -> (CS -> Sym -> r) -> r
genSym s@CompileState{u} f = f s { u = 1 + u } (Sym $ "u" ++ show u)

data CS = CompileState { u :: Int }

----------------------------------------------------------------------
-- Programs

data Prog
  = P_Halt
  | P_SetPixel AtomInt AtomInt (RGB AtomInt) Prog
  | P_Let Sym Rhs Prog
  | P_If AtomBit Prog Prog
  deriving Show

data Rhs
  = R_Form (FormI AtomInt)
  | R_ReadColRom AtomInt
  deriving Show

data AtomInt
  = A_LitI Int
  | A_SymI Sym
  deriving Show

data AtomBit
  = A_LitBit Bool
  | A_Picked Int AtomInt
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

run :: Machine -> Prog -> Picture
run Machine{colRom} = do
  let
    evB :: RS -> AtomBit -> Bool
    evB s = \case
      A_LitBit bool -> bool
      A_Picked index ai -> evI s ai `testBit` index

    evI :: RS -> AtomInt -> Int
    evI RuntimeState{mi}= \case
      A_LitI x -> x
      A_SymI sym -> look sym mi

    evRGB :: RS -> RGB AtomInt -> RGB Int
    evRGB s = fmap (evI s)

    evRhs :: RS -> Rhs -> Int
    evRhs s = \case
      R_Form form -> evFormI (fmap (evI s) form)
      R_ReadColRom a -> fromIntegral $ Rom.lookup colRom (evI s a)
  let
    loop :: RS -> Prog -> RS
    loop s = \case
      P_Halt -> s
      P_SetPixel x y rgb prog -> do
        loop s
          { screen =
            setScreenPixel (screen s) (XY (evI s x) (evI s y)) (evRGB s rgb)
          } prog
      P_Let sym rhs prog -> do
        let val :: Int = evRhs s rhs
        loop s { mi = Map.insert sym val (mi s) } prog
      P_If cond p1 p2 -> do
        let val :: Bool = evB s cond
        loop s (if val then p1 else p2)

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

data Screen = Screen { m :: Map XY (RGB Int)}

screen0 :: Screen
screen0 = Screen { m = Map.empty }

setScreenPixel :: Screen -> XY -> RGB Int -> Screen
setScreenPixel Screen{m} xy rgb = Screen { m = Map.insert xy rgb m }

pictureScreen :: Screen -> Picture
pictureScreen Screen{m} = Pictures [ Draw (shift xy) rgb | (xy,rgb) <- Map.toList m ]
  where
    shift XY{x,y} = XY { x = x+8, y = y+8 }

----------------------------------------------------------------------

data Picture where
  Draw :: XY -> RGB Int -> Picture
  Pictures :: [Picture] -> Picture

data XY = XY { x :: Int, y :: Int } deriving (Eq,Ord,Show)
data RGB a = RGB { r :: a, g :: a, b :: a } deriving (Show,Functor)
