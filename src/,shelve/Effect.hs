
module Effect (Eff(..)) where

import Control.Monad (ap,liftM)
import Data.Word8 (Word8)
import Cpu (Reg16,Reg,Flag)
import HiLo (HiLo(..))
import InstructionSet (Prefix,Op)
import Phase (Byte,Addr,Bit)

-- | The Effect type, constructed by Semantics

data Eff p a where
  Ret :: a -> Eff p a
  Bind :: Eff p a -> (a -> Eff p b) -> Eff p b

  GetReg16 :: Reg16 -> Eff p (Addr p)
  SetReg16 :: Reg16 -> Addr p -> Eff p ()
  GetReg :: Reg -> Eff p (Byte p)
  SetReg :: Reg -> Byte p -> Eff p ()
  GetFlag :: Flag -> Eff p (Bit p)
  SetFlag :: Flag -> Bit p -> Eff p ()

  ReadMem :: Addr p -> Eff p (Byte p)
  WriteMem :: Addr p -> Byte p -> Eff p ()

  PortOutput :: Byte p -> Byte p -> Eff p ()

  IsInterrupt :: Eff p (Maybe (Byte p))

  IsHalted :: Eff p Bool
  SetHalted :: Eff p ()
  SetUnHalted :: Eff p ()

  SetIff1 :: Bool -> Eff p () -- TODO: iff2
  GetIff1 :: Eff p Bool

  SetInterruptMode :: Int -> Eff p ()

  Decode :: Byte p -> Eff p (Either Prefix Op)
  DecodeAfterED :: Byte p -> Eff p Op

  Trace :: Eff p ()
  Advance :: Int -> Eff p ()
  MarkReturnAddress :: Addr p -> Eff p ()

  MakeBit :: Bool -> Eff p (Bit p)
  Flip :: Bit p -> Eff p (Bit p)
  AndBit :: Bit p -> Bit p -> Eff p (Bit p)
  OrBit :: Bit p -> Bit p -> Eff p (Bit p)
  CaseBit :: Bit p -> Eff p Bool

  MakeByte :: Word8 -> Eff p (Byte p)
  ShiftRight :: Byte p -> Byte p -> Eff p (Byte p)
  ShiftLeft :: Byte p -> Byte p -> Eff p (Byte p)
  Complement :: Byte p -> Eff p (Byte p)
  AndB :: Byte p -> Byte p -> Eff p (Byte p)
  OrB :: Byte p -> Byte p -> Eff p (Byte p)
  XorB :: Byte p -> Byte p -> Eff p (Byte p)
  Ite :: Bit p -> Byte p -> Byte p -> Eff p (Byte p)
  AddWithCarry :: Bit p -> Byte p -> Byte p -> Eff p (Byte p, Bit p)
  IsSigned :: Byte p -> Eff p (Bit p)
  IsZero :: Byte p -> Eff p (Bit p)
  IsParity :: Byte p -> Eff p (Bit p)
  TestBit :: Byte p -> Int -> Eff p (Bit p)
  UpdateBit :: Byte p -> Int -> Bit p -> Eff p (Byte p)
  CaseByte :: Byte p -> [Word8] -> Eff p Word8

  MakeAddr :: HiLo (Byte p) -> Eff p (Addr p)
  SplitAddr :: Addr p -> Eff p (HiLo (Byte p))
  OffsetAddr :: Int -> Addr p -> Eff p (Addr p)
  Add16 :: Addr p -> Addr p -> Eff p (Addr p, Bit p)

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind
