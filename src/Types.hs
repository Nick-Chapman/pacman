
module Types (
  Eff(..), System(..),

  Code(..),Step(..),Prog(..), Oper(..),E(..),Reg(..),Var(..),VarId(..),Register(..),
  RomSpec(..), RamSpec(..),

  XY(..),RGB(..), UpDown(..),
  Nat, Bit(..), Key(..), Keys(..),

  Rom,Ram, -- TODO: need rom/ram id

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
  -- | LoadRom RomSpec (Rom -> System)
  -- | LoadRam RamSpec (Ram -> System)
  -- | MapRam Ram Nat System
  -- | MapRom Rom Nat System

-- the core effect type
data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  GetKey :: Key -> Eff UpDown
  SetPixel :: XY (E Nat) -> RGB (E Nat) -> Eff ()

  --Repeat :: Nat -> (Nat -> Eff ()) -> Eff () -- maybe/maybe-dont unroll
  --Nand :: E Bit -> E Bit -> Eff (E Bit)
  --Add :: E Nat -> E Nat -> Eff (E Nat)
  --Concat :: [E a] -> Eff (E [a])
  --Index :: Nat -> E [Bit] -> Eff (E Bit)
  --GetReg :: Reg a -> Eff (E a)
  --SetReg :: Reg a -> E a -> Eff ()
  --ReadMem :: E Nat -> Eff (E Nat)
  --WriteMem :: E Nat -> E Nat -> Eff ()


data UpDown = Up | Down deriving Eq


-- we can perform read/read-write ops on named rom/ram, at constant offsets
-- but to access symbolically (E_Read/WriteMem), we need to set up the mem-mapping
data Rom
data Ram



-- full generated code. includes decs; init; and progs
data Code = Code
  { mm :: [(Either RomSpec RamSpec, Nat)]
  , regs :: [Register]
  , entry :: Prog
  }

-- statement in the generated program, works in context of some decs
data Prog where
  P_Halt :: Prog
  P_Seq :: Step -> Prog -> Prog
  P_If :: E Bool -> Prog -> Prog -> Prog

-- basic program step (statement), which get sequenced in a program
data Step where
  S_Let :: Var a -> Oper a -> Step
  S_SetReg :: Reg a -> E a -> Step
  S_MemWrite :: E Nat -> E Nat -> Step
  S_SetPixel :: XY (E Nat) -> RGB (E Nat) -> Step

-- operation (non atomic expression), will be let-bound
data Oper a where
  O_Nand :: E Bit -> E Bit -> Oper Bit
  O_Add :: E Nat -> E Nat -> Oper Nat
  O_Concat :: E [[Bit]] -> Oper [Bit]
  O_MemRead :: Nat -> Oper Nat

-- program expressions; atomic, so can be freely shared
data E a where
  E_KeyDown :: Key -> E Bool
  E_Lit :: a -> E a
  E_Reg :: Reg a -> E a
  E_Var :: Var a -> E a
  E_Not :: E Bit -> E Bit -- here to allow !! etc to be optimized
  E_Index :: Nat -> E [Bit] -> E Bit -- bounds check at runtime


ePosInt :: Int -> E Nat -- TODO: take required size? (as Nat)
ePosInt = E_Lit . bitsOfInt

--TODO: intro Name a -- for Reg/Var -- dont use terminology Var, bt instead Temp

-- state which persists from one clock cycle to another; a global program variable
data Reg a where
  Reg :: Register -> Reg [Bit]

-- a name for an expression, which may be shared; a local, let-bound variable
data Var a where
  Var :: VarId -> Var [Bit]


data VarId = VarId { size :: Int, u :: Int }

data Register = Register { size :: Int, name :: String }

data Key = KeyEnter | KeyZ | KeyX deriving (Eq,Ord,Enum,Bounded)

newtype Keys = Keys { pressed :: Set Key } deriving Show

data XY a = XY { x :: a, y :: a } deriving (Eq,Ord,Functor)
data RGB a = RGB { r :: a, g :: a, b :: a } deriving (Functor)

data RomSpec = RomSpec { path :: String, size :: Int }
data RamSpec = RamSpec { size :: Int }

type Nat = [Bit] -- helpful?

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
    S_Let{} -> undefined
    S_SetReg{} -> undefined
    S_MemWrite{} -> undefined
    S_SetPixel xy rgb -> show ("SetPixel",xy,rgb)

deriving instance Show a => Show (Oper a)
deriving instance Show a => Show (E a)
deriving instance Show (Var a)
deriving instance Show (Reg a)

deriving instance Show Register
deriving instance Show VarId
deriving instance Show Bit
deriving instance Show Key
deriving instance Show UpDown
deriving instance Show RomSpec
deriving instance Show RamSpec

deriving instance Show a => Show (XY a)
deriving instance Show a => Show (RGB a)
