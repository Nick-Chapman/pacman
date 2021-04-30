
module Types (
  Eff(..), System(..),

  Code(..),Prog(..),Step(..),Oper(..),
  E(..), Name(..),

  RegId(..), SizeSpec(..), Reg(..),
  TmpId(..),
  Tmp(..),

  XY(..),RGB(..),
  Nat, Bit(..), Key(..), Keys(..),

  eNot,
  ePosInt,
  fromBits
  ) where

import Control.Monad (ap,liftM)
import Data.Set (Set)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

-- top level effect; declare regs, load rom etc
data System
  = FrameEffect (Eff ())
  | DeclareReg1 (Reg Bit -> System)

-- the core effect type
data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  CaseBit :: E Bit -> Eff Bit -- TODO: generalize any bounded type
  SetPixel :: XY (E Nat) -> RGB (E Nat) -> Eff ()
  GetReg :: Reg a -> Eff (E a)
  SetReg :: Show a => Reg a -> E a -> Eff ()
  And :: E Bit -> E Bit -> Eff (E Bit)

-- full generated code. includes decs & prog
data Code = Code
  { regDecs :: [(RegId,SizeSpec)]
  , entry :: Prog
  }

-- statement in the generated program, works in context of some decs
data Prog where
  P_Halt :: Prog
  P_Seq :: Step -> Prog -> Prog
  P_If :: E Bit -> Prog -> Prog -> Prog

-- basic program step (statement), which is sequenced in a program
data Step where
  S_Let :: Show a => Tmp a -> Oper a -> Step
  S_SetReg :: Show a => Reg a -> E a -> Step
  S_SetPixel :: XY (E Nat) -> RGB (E Nat) -> Step

-- operation (non atomic/pure expression), will always be let-bound
data Oper a where
  O_Reg :: Reg a -> Oper a
  O_And :: E Bit -> E Bit -> Oper Bit

-- program expressions; atomic/pure, so can be freely shared
data E a where
  E_KeyDown :: Key -> E Bit
  E_Lit :: a -> E a
  E_Not :: E Bit -> E Bit
  E_Tmp :: Tmp a -> E a

eNot :: E Bit -> E Bit
eNot = \case
  E_Lit B1 -> E_Lit B0
  E_Lit B0 -> E_Lit B1
  E_Not ebar -> ebar
  e -> E_Not e

data Name a where
  N_Reg :: Reg a -> Name a

ePosInt :: Int -> E Nat -- TODO: take required size? (as Nat) -- move to Lib.hs
ePosInt = E_Lit . bitsOfInt

data Reg a where
  Reg1 :: RegId -> Reg Bit

data Tmp a where
  Tmp1 :: TmpId -> Tmp Bit

data SizeSpec = SizeSpec { size :: Int }

data RegId = RegId { u :: Int } deriving (Eq,Ord)
data TmpId = TmpId { u :: Int } deriving (Eq,Ord)

----------------------------------------------------------------------
-- show

instance Show Step where
  show = \case
    S_Let tmp oper -> "let " ++ show tmp ++ " = " ++ show oper
    S_SetReg reg exp -> show reg ++ " := " ++ show exp
    S_SetPixel xy rgb -> "set-pixel " ++ show xy ++ " := " ++ show rgb

deriving instance Show (Oper a)
deriving instance Show a => Show (E a)

instance Show (Reg a) where show (Reg1 id) = show id
instance Show (Tmp a) where show (Tmp1 id) = show id

instance Show SizeSpec where show SizeSpec{size} = "#" ++ show size
instance Show RegId where show RegId{u} = "r"++show u
instance Show TmpId where show TmpId{u} = "u"++show u

----------------------------------------------------------------------
-- values

data Key = KeyEnter | KeyZ | KeyX deriving (Eq,Ord,Enum,Bounded,Show)
newtype Keys = Keys { pressed :: Set Key }
data XY a = XY { x :: a, y :: a } deriving (Eq,Ord,Functor)
data RGB a = RGB { r :: a, g :: a, b :: a } deriving (Functor)

type Nat = [Bit]

bitsOfInt :: Int -> [Bit] -- lsb..msb
bitsOfInt n =
  if n < 0 then error "bitsOfInt" else loop n
  where
    loop n = if
      | n == 0 -> []
      | n `mod` 2 == 1 -> B1 : loop (n `div` 2)
      | otherwise -> B0 : loop (n `div` 2)

fromBits :: [Bit] -> Int
fromBits = \case
  [] -> 0
  B0:xs -> 2 * fromBits xs
  B1:xs -> 2 * fromBits xs + 1

data Bit = B0 | B1


instance Show Bit where show = \case B0 -> "0"; B1 -> "1"
instance Show a => Show (XY a) where show XY{x,y} = show (x,y)
instance Show a => Show (RGB a) where show RGB{r,g,b} = "RGB" ++ show (r,g,b)
