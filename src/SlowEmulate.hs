
module SlowEmulate (
  Ticks(..),
  EmuState(..), initState,
  CB(..),
  emulate,
  Bit(..),
  ) where

import Addr (Addr(..),addCarryOut)
import Byte (Byte(..),addWithCarry,parity)
import Cpu (Cpu,Reg(PCL,PCH,F),flagBitPos)
import Data.Bits
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (decode,decodeAfterED)
import Mem (Mem)
import Phase (Phase)
import Text.Printf (printf)
import qualified Addr (fromHiLo,toHiLo,bump)
import qualified Byte (toUnsigned)
import qualified Cpu (init,get16,set16,get,set)
import qualified Mem (init,readIO,writeIO)
import qualified Mem (read)
import qualified Phase (Byte,Addr,Bit)
import qualified Semantics (fetchDecodeExec)

-- | Clock ticks
newtype Ticks = Ticks { unTicks :: Int } deriving (Eq,Ord,Num)

instance Show Ticks where show = printf "%d" . unTicks

data EmuTime -- At Emulation type we have concrete Bytes

instance Phase EmuTime where
  type Byte EmuTime = Byte
  type Addr EmuTime = Addr
  type Bit EmuTime = Bit

newtype Bit = Bit Bool
instance Show Bit where show (Bit b) = if b then "1" else "0"

data EmuState = EmuState
  { ticks :: Ticks -- cycle count -- TODO: move inside cpu
  , icount :: Int -- instruction count -- KILL
  , cpu :: Cpu EmuTime
  , mem :: Mem
  , interrupts_enabled :: Bool -- TODO: move inside cpu
  }

instance Show EmuState where
  show EmuState{ticks,cpu,mem} = do
    unwords
      [ printf "cyc: %3s, " (show ticks)
      , show cpu
      , "(" ++ unwords [ show b | a <- take 4 [programCounter cpu ..], let b = Mem.read mem a ] ++ ")"
      ]

programCounter :: Cpu EmuTime -> Addr
programCounter cpu = do
  let lo = Cpu.get cpu PCL
  let hi = Cpu.get cpu PCH
  Addr.fromHiLo HiLo{hi,lo}

initState :: IO EmuState
initState = do
  mem <- Mem.init
  pure $ EmuState
    { ticks = 0
    , icount = 0
    , cpu = cpu0
    , mem
    , interrupts_enabled = False
    }

cpu0 :: Cpu EmuTime
cpu0 = Cpu.init (Addr 0) (Addr 0xFFFF) (Byte 0) (Byte 0xFF)

data CB = CB { trace :: EmuState -> IO () }

emulate :: CB -> EmuState -> IO EmuState
emulate CB{trace} s0 = do
  run s0 Semantics.fetchDecodeExec $ \post () -> return post
  where
    run :: EmuState -> Eff EmuTime a -> (EmuState -> a -> IO EmuState) -> IO EmuState
    run s@EmuState{ticks,cpu,mem} eff k = case eff of
      Ret x -> k s x
      Bind eff f -> run s eff $ \s a -> run s (f a) k

      GetReg16 rr -> k s (Cpu.get16 cpu rr)
      SetReg16 rr a -> k s { cpu = Cpu.set16 cpu rr a} ()
      GetReg r -> k s (Cpu.get cpu r)
      SetReg r b -> k s { cpu = Cpu.set cpu r b} ()

      GetFlag flag -> do -- TODO: move into Semantics; remove as effect
        let byte = Cpu.get cpu F
        let pos = flagBitPos flag
        let bit = Bit (byte `testBit` pos)
        k s bit

      SetFlag flag (Bit bool) -> do -- TODO: move into Semantics; remove as effect
        let byte = Cpu.get cpu F
        let pos = flagBitPos flag
        let byte' = (if bool then setBit else clearBit) byte pos
        k s { cpu = Cpu.set cpu F byte' } ()

      ReadMem a -> do b <- Mem.readIO mem a; k s b
      WriteMem a b -> do mem <- Mem.writeIO mem a b; k s { mem } ()

      EnableInterrupts -> k s { interrupts_enabled = True } ()
      DisableInterrupts -> k s { interrupts_enabled = False } ()
      Decode byte -> k s (decode byte)
      DecodeAfterED byte -> k s (decodeAfterED byte)
      MarkReturnAddress {} -> k s ()
      Trace -> do trace s; k s { icount = icount s + 1 } ()
      Advance n -> k s { ticks = ticks + fromIntegral n } ()

      MakeBit (bool) -> k s (Bit bool)
      Flip (Bit bool) -> k s (Bit (not bool))
      AndBit (Bit b1) (Bit b2) -> k s (Bit (b1 && b2))
      OrBit (Bit b1) (Bit b2) -> k s (Bit (b1 || b2))
      CaseBit (Bit bool) -> k s bool

      MakeByte w -> k s (Byte w)
      ShiftRight byte offset -> k s (byte `shiftR` (Byte.toUnsigned offset))
      ShiftLeft byte offset -> k s (byte `shiftL` (Byte.toUnsigned offset))
      Complement b -> k s (complement b)
      AndB b1 b2 -> k s (b1 .&. b2)
      OrB b1 b2 -> k s (b1 .|. b2)
      XorB b1 b2 -> k s (b1 `xor` b2)
      Ite (Bit i) t e -> k s (if i then t else e)
      AddWithCarry (Bit cin) v1 v2 -> do
        let (v,cout) = Byte.addWithCarry cin v1 v2
        k s (v, Bit cout)
      IsSigned byte -> k s (Bit (byte `testBit` 7))
      IsZero byte -> k s (Bit (byte == 0))
      IsParity byte -> do k s (Bit (Byte.parity  byte))
      TestBit byte i -> k s (Bit (byte `testBit` i))
      UpdateBit byte i (Bit bool) -> k s ((if bool then setBit else clearBit) byte i)
      CaseByte (Byte word) _choices -> k s word

      MakeAddr hilo -> k s (Addr.fromHiLo hilo)
      SplitAddr a -> k s (Addr.toHiLo a)
      OffsetAddr n a -> k s (Addr.bump a n)
      Add16 w1 w2 -> do
        let (w, cout) = Addr.addCarryOut w1 w2
        k s (w, Bit cout)
