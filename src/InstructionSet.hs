
module InstructionSet (
  Prefix(..),Op(..),Op0(..),Op1(..),Op2(..), RegPairSpec(..), RegSpec(..), Condition(..),
  Instruction(..), justOp,
  cycles,
  decodeAfterED,
  decode,
  theDecodeTable,
  dis1
  ) where

import Byte (Byte(..))
import Data.List (sort)
import Data.List.Extra (groupSort)
import Data.Map (Map)
import Data.Word8 (Word8)
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

data Prefix = PrefixED

-- | Op-codes. Stratified by the number of following immediate bytes
data Op = Op0 Op0 | Op1 Op1 | Op2 Op2
  deriving (Eq,Ord,Show)

-- | Ops which take zero immediate bytes. Listed in encoding order
data Op0
  = NOP
  | STAX RegPairSpec
  | INX RegPairSpec
  | INR RegSpec
  | DCR RegSpec
  | RLC
  | RAL
  | DAA
  | STC
  | DAD RegPairSpec
  | LDAX RegPairSpec
  | DCX RegPairSpec
  | RRC
  | RAR
  | CMA
  | CMC
  | MOV { dest :: RegSpec, src :: RegSpec }
  | HLT
  | ADD RegSpec
  | ADC RegSpec
  | SUB RegSpec
  | SBB RegSpec
  | ANA RegSpec
  | XRA RegSpec
  | ORA RegSpec
  | CMP RegSpec
  | RCond Condition
  | POP RegPairSpec
  | XTHL
  | DI
  | PUSH RegPairSpec
  | RST Word8 --(0..7)
  | RET
  | PCHL
  | SPHL
  | XCHG
  | EI
  | IM2
  | EXX
  deriving (Eq,Ord,Show)

-- | Ops which take one immediate byte.
data Op1
  = MVI RegSpec
  | OUT
  | ADI
  | SUI -- subtract
  | ANI
  | ORI
  | IN
  | ACI
  | SBI -- subtract with borrow
  | XRI
  | CPI
  | DJNZ
  | JR Condition
  deriving (Eq,Ord,Show)

-- | Ops which take two immediate bytes.
data Op2
  = LXI RegPairSpec
  | SHLD
  | STA
  | LHLD
  | LDA
  | JCond Condition
  | JMP
  | CCond Condition
  | CALL
  deriving (Eq,Ord,Show)

data Condition = NZ | NC | PO | P | Z | CY | PE | MI
  deriving (Eq,Ord,Show)

data RegSpec = A | B | C | D | E | H | L | M | I
  deriving (Eq,Ord,Show)

data RegPairSpec = BC | DE | HL | SP | PSW
  deriving (Eq,Ord,Show)

allOps :: [Op]
allOps = map Op0 all0 ++ map Op1 all1 ++ map Op2 all2
  where
    all0 =
      [NOP,RLC,RAL,DAA,STC,RRC,RAR,CMA,CMC,HLT,XTHL,DI,RET,PCHL,SPHL,XCHG,EI,EXX]
      ++ [ op p | op <- [STAX,LDAX], p <- [BC,DE] ]
      ++ [ op r | op <- [INR,DCR,ADD,ADC,SUB,SBB,ANA,XRA,ORA,CMP], r <- regs ]
      ++ [ op p | op <- [INX,DAD,DCX], p <- rps1 ]
      ++ [ op p | op <- [POP,PUSH], p <- rps2 ]
      ++ [ MOV {dest,src} | dest <- regs, src <- regs, not (dest==M && src==M) ]
      ++ [ RCond c | c <- conds ]
      ++ [ RST n | n <- [0..7] ]
    all1 =
      [ADI,SUI,ANI,ORI,ACI,SBI,XRI,CPI,OUT,IN] ++ [ MVI r | r <- regs ]
      ++ [DJNZ]
      ++ [JR NZ] -- [ JR c | c <- [Z,NZ,CY,NC] ] -- TODO: when encountered
    all2 =
      [SHLD,STA,LHLD,LDA,JMP,CALL]
      ++ [ LXI r | r <- rps1]
      ++ [ op c | op <- [CCond,JCond], c <- conds]

    regs = [A,B,C,D,E,H,L,M]
    rps1 = [BC,DE,HL,SP]
    rps2 = [BC,DE,HL,PSW]
    conds= [Z,NZ,CY,NC,PO,PE,P,MI]

cycles :: Bool -> Op -> Int
cycles jumpTaken = \case
  Op0 NOP -> 4
  Op2 LXI{} -> 10
  Op0 STAX{} -> 7
  Op2 SHLD -> 16
  Op2 STA -> 13
  Op0 INX{} -> 5
  Op0 (INR r) -> mcost r 4 10 --8080: was 5
  Op0 (DCR r) -> mcost r 4 10 --8080: was 5
  Op1 (MVI r) -> mcost r 7 10
  Op0 RLC -> 4
  Op0 RAL -> 4
  Op0 DAA -> 4
  Op0 STC -> 4
  Op0 DAD{} -> 10
  Op0 LDAX{} -> 7
  Op2 LHLD -> 16
  Op2 LDA -> 13
  Op0 DCX{} -> 6 --8080: was 5
  Op0 RRC -> 4
  Op0 RAR -> 4
  Op0 CMA -> 4
  Op0 CMC -> 4
  Op0 HLT -> 4 -- 8080: was 7
  Op0 MOV {dest=M,src=M} -> error "illegal instruction: MOV M,M"
  Op0 MOV {src=M} -> 7
  Op0 MOV {dest=M} -> 7
  Op0 MOV{} -> 4 --8080: was 5
  Op0 (ADD r) -> mcost r 4 7
  Op0 (ADC r) -> mcost r 4 7
  Op0 (SUB r) -> mcost r 4 7
  Op0 (SBB r) -> mcost r 4 7
  Op0 (ANA r) -> mcost r 4 7
  Op0 (XRA r) -> mcost r 4 7
  Op0 (ORA r) -> mcost r 4 7
  Op0 (CMP r) -> mcost r 4 7
  Op0 RCond{} -> if jumpTaken then 11 else 5
  Op0 POP{} -> 10
  Op2 JCond{} -> 10
  Op2 JMP -> 10
  Op1 OUT -> 11 --8080: was 10
  Op0 XTHL -> 18
  Op0 DI -> 4
  Op2 CCond{} -> if jumpTaken then 17 else 11
  Op0 PUSH{} -> 11
  Op1 ADI -> 7
  Op1 SUI -> 7
  Op1 ANI -> 7
  Op1 ORI -> 7
  Op0 RST{} -> 4
  Op0 RET -> 10
  Op0 PCHL -> 5
  Op0 SPHL -> 5
  Op1 IN -> 10
  Op0 XCHG -> 4
  Op0 EI -> 4
  Op2 CALL -> 17
  Op1 ACI -> 7
  Op1 SBI -> 7
  Op1 XRI -> 7
  Op1 CPI -> 7
  Op1 DJNZ -> if jumpTaken then 13 else 8
  Op1 JR{} -> if jumpTaken then 12 else 7
  Op0 IM2 -> 3 -- not including +5 for ED prefix
  Op0 EXX -> 4

mcost :: RegSpec -> Int -> Int -> Int
mcost x a b = case x of M -> b; _ -> a

data Instruction b -- op+args
  = Ins0 Op0
  | Ins1 Op1 b
  | Ins2 Op2 b b
  deriving (Functor,Show)

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

justOp :: Instruction b -> Op
justOp = \case
  Ins0 op0 -> Op0 op0
  Ins1 op1 _ -> Op1 op1
  Ins2 op2 _ _ -> Op2 op2

encode :: Op -> Byte
encode = \case
  Op0 NOP -> 0x00
  Op2 (LXI rp) -> Byte (16 * encodeRegPairSpec rp + 0x1)
  Op2 SHLD -> 0x22
  Op2 STA -> 0x32
  Op0 (STAX rp) -> Byte (16 * encodeRegPairSpec rp + 0x2)
  Op0 (INX rp) -> Byte (16 * encodeRegPairSpec rp + 0x3)
  Op0 (INR reg) -> Byte (8 * encodeRegSpec reg + 0x04)
  Op0 (DCR reg) -> Byte (8 * encodeRegSpec reg + 0x05)
  Op1 (MVI dest) -> Byte (8 * encodeRegSpec dest + 0x06)
  Op0 RLC -> 0x07
  Op0 RAL -> 0x17
  Op0 DAA -> 0x27
  Op0 STC -> 0x37
  Op0 (DAD rp) -> Byte (16 * encodeRegPairSpec rp + 0x9)
  Op0 (LDAX rp) -> Byte (16 * encodeRegPairSpec rp + 0x0A)
  Op2 LHLD -> 0x2A
  Op2 LDA -> 0x3A
  Op0 (DCX rp) -> Byte (16 * encodeRegPairSpec rp + 0xB)
  Op0 RRC -> 0x0F
  Op0 RAR -> 0x1F
  Op0 CMA -> 0x2F
  Op0 CMC -> 0x3F
  Op0 MOV {dest,src} -> Byte (0x40 + 8 * encodeRegSpec dest + encodeRegSpec src)
  Op0 HLT -> 0x76
  Op0 (ADD reg) -> Byte (encodeRegSpec reg + 0x80)
  Op0 (ADC reg) -> Byte (encodeRegSpec reg + 0x88)
  Op0 (SUB reg) -> Byte (encodeRegSpec reg + 0x90)
  Op0 (SBB reg) -> Byte (encodeRegSpec reg + 0x98)
  Op0 (ANA reg) -> Byte (encodeRegSpec reg + 0xA0)
  Op0 (XRA reg) -> Byte (encodeRegSpec reg + 0xA8)
  Op0 (ORA reg) -> Byte (encodeRegSpec reg + 0xB0)
  Op0 (CMP reg) -> Byte (encodeRegSpec reg + 0xB8)
  Op0 (RCond cond) -> Byte (8 * encodeCondition cond + 0xC0)
  Op0 (POP rp) -> Byte (16 * encodeRegPairSpec rp + 0xC1)
  Op2 (JCond cond) -> Byte (8 * encodeCondition cond + 0xC2)
  Op2 JMP -> 0xC3
  Op1 OUT -> 0xD3
  Op0 XTHL -> 0xE3
  Op0 DI -> 0xF3
  Op2 (CCond cond) -> Byte (8 * encodeCondition cond + 0xC4)
  Op0 (PUSH rp) -> Byte (16 * encodeRegPairSpec rp + 0xC5)
  Op1 ADI -> 0xC6
  Op1 SUI -> 0xD6
  Op1 ANI -> 0xE6
  Op1 ORI -> 0xF6
  Op0 (RST n) -> Byte (8 * fromIntegral n + 0xC7)
  Op0 RET -> 0xC9
  Op0 PCHL -> 0xE9
  Op0 SPHL -> 0xF9
  Op1 IN -> 0xDB
  Op0 XCHG -> 0xEB
  Op0 EI -> 0xFB
  Op2 CALL -> 0xCD
  Op1 ACI -> 0xCE
  Op1 SBI -> 0xDE
  Op1 XRI -> 0xEE
  Op1 CPI -> 0xFE
  Op1 DJNZ -> 0x10
  Op1 (JR cond) -> Byte (8 * encodeCondition cond + 0x20)
  Op0 IM2 -> 0x5E -- TODO: needs to be here?
  Op0 EXX -> 0xD9

encodeCondition :: Condition -> Word8
encodeCondition = \case
  NZ -> 0
  Z -> 1
  NC -> 2
  CY -> 3
  PO -> 4
  PE -> 5
  P -> 6
  MI -> 7

encodeRegSpec :: RegSpec -> Word8
encodeRegSpec = \case
  B -> 0
  C -> 1
  D -> 2
  E -> 3
  H -> 4
  L -> 5
  M -> 6
  A -> 7
  I -> error "encodeRegSpec:I"

encodeRegPairSpec :: RegPairSpec -> Word8
encodeRegPairSpec = \case
  BC -> 0
  DE -> 1
  HL -> 2
  -- SP & PSW share same encoding
  SP -> 3
  PSW -> 3 -- for PUSH/POP

decodeAfterED :: Byte -> Op
decodeAfterED = \case
  0x47 -> Op0 (MOV {src=A,dest=I})
  0x5E -> Op0 IM2
  byte -> error (show ("decodeAfterED",byte))

-- | define decode as the inverse of encoding
decode :: Byte -> Either Prefix Op
decode = \case
  0xED -> Left PrefixED
  byte ->
    case Map.lookup byte map of
      Nothing -> error $ "decode: " <> show byte
      Just op -> Right op
    where (DecodeTable map) = theDecodeTable

newtype DecodeTable = DecodeTable (Map Byte Op)

theDecodeTable :: DecodeTable
theDecodeTable = DecodeTable $ Map.fromList ys
  where
    xs = [ (encode op, op) | op <- allOps ]
    ys = [ (k,expectUnique k vs) | (k,vs) <- groupSort xs ]
    expectUnique k = \case
      [op] -> op
      ops -> error $
        unlines $
        ("bad decoding: " <> show k)
          : [ "--> " <> ljust 13 (show (docInstructionForOp op)) <> " [" <> show op <> "]" | op <- ops ]

instance Show DecodeTable where
  show (DecodeTable map) =
    unlines
    [ unlines [ show k <> " --> " <> show (docInstructionForOp v) | (k,v) <- ps ]
    , printf "implemented: %d, unimplemented %d" n (256-n)
    ]
    where
      ps = sort (Map.toList map)
      n = length ps

docInstructionForOp :: Op -> Instruction ImmSpec
docInstructionForOp = \case
  Op0 op0 -> Ins0 op0
  Op1 op1 -> Ins1 op1 B1
  Op2 op2 -> Ins2 op2 B1 B2

data ImmSpec = B1 | B2

instance Show ImmSpec where
  show = \case
    B1 -> "b1"
    B2 -> "b2"

dis1 :: [Byte] -> Op
dis1 = \case
  b0:b1:_ ->
    case decode b0 of
      Left PrefixED -> decodeAfterED b1
      Right op -> op
  _ ->
    error "dis1"
