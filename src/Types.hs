
module Types (
  Eff(..), System(..),

  Code(..),Prog(..),Step(..),Oper(..),
  E(..), Name(..),

  RegId(..), RegSpec(..), Reg(..),
  TmpId(..), TmpSpec(..), Tmp(..),

  XY(..),RGB(..),
  Nat, Bit(..), Key(..), Keys(..),

  --RomSpec(..), RamSpec(..),
  --Rom,Ram, -- TODO: need rom/ram id

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
  -- | LoadRom RomSpec (Rom -> System)
  -- | LoadRam RamSpec (Ram -> System)
  -- | MapRam Ram Nat System
  -- | MapRom Rom Nat System

-- the core effect type
data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  CaseBit :: E Bit -> Eff Bit -- TODO: generalize any bounded type
  KeyDown :: Key -> Eff (E Bit)
  SetPixel :: XY (E Nat) -> RGB (E Nat) -> Eff ()
  GetReg :: Reg a -> Eff (E a) -- TODO: does this have to be in Eff -- YES for inlining
  SetReg :: Show a => Reg a -> E a -> Eff ()
  Not :: E Bit -> Eff (E Bit) -- TODO: move out of effect type!
  And :: E Bit -> E Bit -> Eff (E Bit)

  --Repeat :: Nat -> (Nat -> Eff ()) -> Eff () -- maybe/maybe-dont unroll
  --Add :: E Nat -> E Nat -> Eff (E Nat)
  --Concat :: [E a] -> Eff (E [a])
  --Index :: Nat -> E [Bit] -> Eff (E Bit)
  --ReadMem :: E Nat -> Eff (E Nat)
  --WriteMem :: E Nat -> E Nat -> Eff ()


-- we can perform read/read-write ops on named rom/ram, at constant offsets
-- but to access symbolically (E_Read/WriteMem), we need to set up the mem-mapping
--data Rom
--data Ram


-- full generated code. includes decs; init; and progs
data Code = Code
  { regDecs :: [(RegId,RegSpec)]
--  , mm :: [(Either RomSpec RamSpec, Nat)]
  , entry :: Prog
  }

-- statement in the generated program, works in context of some decs
data Prog where
  P_Halt :: Prog
  P_Seq :: Step -> Prog -> Prog
  P_If :: E Bool -> Prog -> Prog -> Prog

-- basic program step (statement), which get sequenced in a program
data Step where
  S_Let :: Show a => Tmp a -> Oper a -> Step
  S_SetReg :: Show a => Reg a -> E a -> Step
  --S_MemWrite :: E Nat -> E Nat -> Step
  S_SetPixel :: XY (E Nat) -> RGB (E Nat) -> Step

-- operation (non atomic expression), will be let-bound
data Oper a where
  O_And :: E Bit -> E Bit -> Oper Bit
  --O_Add :: E Nat -> E Nat -> Oper Nat
  --O_Concat :: E [[Bit]] -> Oper [Bit]
  O_MemRead :: Nat -> Oper Nat

-- program expressions; atomic, so can be freely shared
data E a where
  E_KeyDown :: Key -> E Bit
  E_TestBit :: E Bit -> E Bool
  E_Lit :: a -> E a
  E_Reg :: Reg a -> E a
  E_Not :: E Bit -> E Bit -- here to allow !! etc to be optimized
  E_Tmp :: Tmp a -> E a
  --E_Index :: Nat -> E [Bit] -> E Bit -- bounds check at runtime

data Name a where
  N_Reg :: Reg a -> Name a

ePosInt :: Int -> E Nat -- TODO: take required size? (as Nat) -- move to Lib.hs
ePosInt = E_Lit . bitsOfInt

data Reg a where
  --Reg :: RegSpec -> RegId -> Reg [Bit]
  Reg1 :: RegId -> Reg Bit

data RegSpec = RegSpec { size :: Int }
data RegId = RegId { u :: Int } deriving (Eq,Ord)

data Tmp a where
  --Tmp :: TmpSpec -> TmpId -> Tmp [Bit]
  Tmp1 :: TmpId -> Tmp Bit

data TmpSpec = TmpSpec { size :: Int }
data TmpId = TmpId { u :: Int } deriving (Eq,Ord)



data Key = KeyEnter | KeyZ | KeyX deriving (Eq,Ord,Enum,Bounded)
newtype Keys = Keys { pressed :: Set Key } deriving Show
data XY a = XY { x :: a, y :: a } deriving (Eq,Ord,Functor)
data RGB a = RGB { r :: a, g :: a, b :: a } deriving (Functor)

--data RomSpec = RomSpec { path :: String, size :: Int }
--data RamSpec = RamSpec { size :: Int }

type Nat = [Bit]

data Bit = B0 | B1
  deriving Eq

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

----------------------------------------------------------------------

deriving instance Show Code
deriving instance Show Prog

instance Show Step where
  show = \case
    S_Let tmp oper -> show ("S_Let",tmp,oper)
    S_SetReg reg exp -> show ("SetReg",reg,exp)
    --S_MemWrite{} -> undefined
    S_SetPixel xy rgb -> show ("SetPixel",xy,rgb)

deriving instance Show a => Show (Oper a)
deriving instance Show a => Show (E a)

deriving instance Show (Tmp a)
deriving instance Show TmpId

deriving instance Show (Reg a)
deriving instance Show RegSpec
deriving instance Show RegId

deriving instance Show Bit
deriving instance Show Key
--deriving instance Show RomSpec
--deriving instance Show RamSpec

deriving instance Show a => Show (XY a)
deriving instance Show a => Show (RGB a)
