
module ZEmu (
  Interaction(..), interaction,
  State, programCounter,
  ) where

import Addr (Addr(..),addCarryOut)
import Byte (Byte(..),addWithCarry,parity)
import Cpu (Cpu,Reg(PCL,PCH,F),flagBitPos)
import Data.Bits
import Effect (Eff)
import HiLo (HiLo(..))
import InstructionSet (decode,decodeAfterED)
import Phase (Phase)
import qualified Addr (fromHiLo,toHiLo,bump)
import qualified Byte (toUnsigned)
import qualified Cpu (init,get16,set16,get,set)
import qualified Effect as E (Eff(..))
import qualified Phase (Byte,Addr,Bit)
import qualified Semantics (fetchDecodeExec)

data EmuTime -- At Emulation time we have concrete Bytes

instance Phase EmuTime where
  type Byte EmuTime = Byte
  type Addr EmuTime = Addr
  type Bit EmuTime = Bit

newtype Bit = Bit Bool
instance Show Bit where show (Bit b) = if b then "1" else "0"

data Interaction
  = Trace State Interaction
  | ReadMem Addr (Byte -> Interaction)
  | WriteMem Addr Byte Interaction
  | OutputPort Byte Byte Interaction
  | Advance Int (Maybe PendingInterrupt -> Interaction)

type PendingInterrupt = Byte

data State = State
  { cpu :: Cpu EmuTime
  , halted :: Bool
  , interrupts_enabled :: Bool -- TODO: move inside cpu?
  , interrupt_mode :: Int  -- TODO: move inside cpu?
  , interrupt_data :: Maybe Byte
  }

instance Show State where
  show State{cpu} = show cpu

programCounter :: State -> Addr
programCounter State{cpu} = do
  let lo = Cpu.get cpu PCL
  let hi = Cpu.get cpu PCH
  Addr.fromHiLo HiLo{hi,lo}

initState :: State
initState = do
  State
    { cpu = cpu0
    , halted = False
    , interrupts_enabled = False
    , interrupt_mode = 0 -- TODO: ?
    , interrupt_data = Nothing
    }

cpu0 :: Cpu EmuTime
cpu0 = Cpu.init (Addr 0) (Addr 0xFFFF) (Byte 0) (Byte 0xFF)

interaction :: Interaction
interaction = emulate initState
  where

    emulate :: State -> Interaction
    emulate s = run s Semantics.fetchDecodeExec $ \s () -> emulate s

    run :: State -> Eff EmuTime a -> (State -> a -> Interaction) -> Interaction
    run s@State{cpu} eff k = case eff of
      E.Ret x -> k s x
      E.Bind eff f -> run s eff $ \s a -> run s (f a) k

      E.GetReg16 rr -> k s (Cpu.get16 cpu rr)
      E.SetReg16 rr a -> k s { cpu = Cpu.set16 cpu rr a} ()
      E.GetReg r -> k s (Cpu.get cpu r)
      E.SetReg r b -> k s { cpu = Cpu.set cpu r b} ()

      E.GetFlag flag -> do -- TODO: move into Semantics; remove as effect
        let byte = Cpu.get cpu F
        let pos = flagBitPos flag
        let bit = Bit (byte `testBit` pos)
        k s bit

      E.SetFlag flag (Bit bool) -> do -- TODO: move into Semantics; remove as effect
        let byte = Cpu.get cpu F
        let pos = flagBitPos flag
        let byte' = (if bool then setBit else clearBit) byte pos
        k s { cpu = Cpu.set cpu F byte' } ()

      E.ReadMem a -> ReadMem a $ (k s)
      E.WriteMem a b -> WriteMem a b (k s ())

      E.PortOutput p v -> do
        OutputPort p v (k s ())

      E.IsInterrupt -> k s { interrupt_data = Nothing } (interrupt_data s)

      E.IsHalted -> k s (halted s)
      E.SetHalted -> k s { halted = True } ()
      E.SetUnHalted -> k s { halted = False } ()

      E.EnableInterrupts -> k s { interrupts_enabled = True } ()
      E.DisableInterrupts -> k s { interrupts_enabled = False } ()
      E.SetInterruptMode i -> k s { interrupt_mode = i } ()

      E.Decode byte -> k s (decode byte)
      E.DecodeAfterED byte -> k s (decodeAfterED byte)
      E.MarkReturnAddress {} -> k s ()

      E.Trace -> Trace s (k s ())

      E.Advance n -> do
        Advance n $ \interrupt_data ->
          k s { interrupt_data } ()

      E.MakeBit (bool) -> k s (Bit bool)
      E.Flip (Bit bool) -> k s (Bit (not bool))
      E.AndBit (Bit b1) (Bit b2) -> k s (Bit (b1 && b2))
      E.OrBit (Bit b1) (Bit b2) -> k s (Bit (b1 || b2))
      E.CaseBit (Bit bool) -> k s bool

      E.MakeByte w -> k s (Byte w)
      E.ShiftRight byte offset -> k s (byte `shiftR` (Byte.toUnsigned offset))
      E.ShiftLeft byte offset -> k s (byte `shiftL` (Byte.toUnsigned offset))
      E.Complement b -> k s (complement b)
      E.AndB b1 b2 -> k s (b1 .&. b2)
      E.OrB b1 b2 -> k s (b1 .|. b2)
      E.XorB b1 b2 -> k s (b1 `xor` b2)
      E.Ite (Bit i) t e -> k s (if i then t else e)
      E.AddWithCarry (Bit cin) v1 v2 -> do
        let (v,cout) = Byte.addWithCarry cin v1 v2
        k s (v, Bit cout)
      E.IsSigned byte -> k s (Bit (byte `testBit` 7))
      E.IsZero byte -> k s (Bit (byte == 0))
      E.IsParity byte -> do k s (Bit (Byte.parity  byte))
      E.TestBit byte i -> k s (Bit (byte `testBit` i))
      E.UpdateBit byte i (Bit bool) -> k s ((if bool then setBit else clearBit) byte i)
      E.CaseByte (Byte word) _choices -> k s word

      E.MakeAddr hilo -> k s (Addr.fromHiLo hilo)
      E.SplitAddr a -> k s (Addr.toHiLo a)
      E.OffsetAddr n a -> k s (Addr.bump a n)
      E.Add16 w1 w2 -> do
        let (w, cout) = Addr.addCarryOut w1 w2
        k s (w, Bit cout)
