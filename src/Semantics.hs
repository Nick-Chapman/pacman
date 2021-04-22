
-- | Execution semantics of Z80 instructions

module Semantics (fetchDecodeExec) where

import Cpu (Reg16,Reg)
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Prefix(..),Op(..),Instruction(..),Op0(..),Op1(..),Op2(..),RegPairSpec(..),Condition(..),cycles,justOp)
import InstructionSet (RegSpec(..))
import Phase (Addr,Byte,Bit)
import Prelude hiding (subtract)
import qualified Cpu

fetchDecodeExec :: Eff p ()
fetchDecodeExec = do
  Trace
  IsInterrupt >>= \case
    Nothing -> do
      op <- fetchOp
      i <- fetchImmediates op
      execInstruction i
    Just byte -> do
      -- TODO: check interrupt mode
      SetUnHalted
      Advance 23 -- TODO: ?
      hi <- GetReg Cpu.I
      vec0 <- MakeAddr $ HiLo{hi,lo = byte}
      vec1 <- OffsetAddr 1 vec0
      hi <- ReadMem vec1
      lo <- ReadMem vec0
      dest <- MakeAddr $ HiLo{hi,lo}
      pushReturn
      setPC dest

fetchOp :: Eff p Op
fetchOp = do
  IsHalted >>= \case
    True -> pure (Op0 NOP)
    False -> do
      byte <- fetch
      Decode byte >>= \case
        Left PrefixED -> do
          Advance 5 -- TODO:??
          byte2 <- fetch
          DecodeAfterED byte2
        Right op -> do
          pure op

fetchImmediates :: Op -> Eff p (Instruction (Byte p))
fetchImmediates = \case
  Op0 op0 -> return (Ins0 op0)
  Op1 op1 -> do
    b1 <- fetch
    return (Ins1 op1 b1)
  Op2 op2 -> do
    b1 <- fetch
    b2 <- fetch
    return (Ins2 op2 b1 b2)

execInstruction :: Instruction (Byte p) -> Eff p ()
execInstruction instruction = do
  n <- execute instruction >>= \case
    Next -> do
      return $ cycles False (justOp instruction)
    Jump a -> do
      setPC a
      return $ cycles True (justOp instruction)
  Advance n

fetch :: Eff p (Byte p) -- fetch byte at PC, and increment PC
fetch = do
  pc <- getPC
  byte <- ReadMem pc
  OffsetAddr 1 pc >>= setPC
  return byte


data Flow p = Next | Jump (Addr p)

execute :: Instruction (Byte p) -> Eff p (Flow p)
execute = \case
  Ins0 op0 -> execute0 op0
  Ins1 op1 b1 -> execute1 op1 b1
  Ins2 op2 lo hi -> do
    a <- MakeAddr $ HiLo{hi,lo}
    execute2 op2 a

execute0 :: Op0 -> Eff p (Flow p)
execute0 = \case
  NOP -> do
    return Next
  {-NOPx{} -> do
    return Next-}
  STAX rp -> do
    a <- load16 rp
    b <- load A
    WriteMem a b
    return Next
  INX rp -> do
    a <- load16 rp
    a' <- OffsetAddr 1 a
    save16 rp a'
    return Next
  INR reg -> do
    v0 <- load reg
    cin <- MakeBit True
    zero <- MakeByte 0
    (v,_coutIgnored) <- AddWithCarry cin v0 zero
    aux <- carryBit 4 cin v0 zero
    SetFlag Cpu.HF aux
    o <- overflows cin v0 zero
    SetFlag Cpu.PF o
    saveAndSetFlagsFrom reg v
    return Next
  DCR reg -> do
    v0 <- load reg
    cin <- MakeBit True
    zero <- MakeByte 0
    (v,_borrowIgnored) <- subWithCarry cin v0 zero
    saveAndSetFlagsFrom reg v
    return Next
  RLC -> do
    byte <- load A
    one <- MakeByte 1
    shunted <- byte `TestBit` 7
    shifted <- byte `ShiftLeft` one
    rotated <- UpdateBit shifted 0 shunted
    SetFlag Cpu.CF shunted
    save A rotated
    return Next
  RAL -> do
    byte <- load A
    one <- MakeByte 1
    shunted <- byte `TestBit` 7
    shifted <- byte `ShiftLeft` one
    cin <- GetFlag Cpu.CF
    rotated <- UpdateBit shifted 0 cin
    SetFlag Cpu.CF shunted
    save A rotated
    return Next
  DAA -> do
    byteIn <- load A
    auxIn <- GetFlag Cpu.HF
    cin <- GetFlag Cpu.CF
    (byteOut,auxOut,cout) <- decimalAdjust auxIn cin byteIn
    SetFlag Cpu.HF auxOut
    SetFlag Cpu.CF cout
    save A byteOut
    o <- MakeBit False -- TODO: ???
    SetFlag Cpu.PF o
    setFlagsFrom byteOut
    return Next
  STC -> do
    bit <- MakeBit True
    SetFlag Cpu.CF bit
    return Next
  DAD rp -> do
    w1 <- load16 rp
    w2 <- load16 HL
    (w,cout) <- Add16 w1 w2
    save16 HL w
    SetFlag Cpu.CF cout
    return Next
  LDAX rp -> do
    a <- load16 rp
    b <- ReadMem a
    save A b
    return Next
  DCX rp -> do
    a <- load16 rp
    a' <- OffsetAddr (-1) a
    save16 rp a'
    return Next
  RRC -> do
    byte <- load A
    one <- MakeByte 1
    shunted <- byte `TestBit` 0
    shifted <- byte `ShiftRight` one
    rotated <- UpdateBit shifted 7 shunted
    SetFlag Cpu.CF shunted
    save A rotated
    return Next
  RAR -> do
    byte <- load A
    one <- MakeByte 1
    shunted <- byte `TestBit` 0
    shifted <- byte `ShiftRight` one
    cin <- GetFlag Cpu.CF
    rotated <- UpdateBit shifted 7 cin
    SetFlag Cpu.CF shunted
    save A rotated
    return Next
  CMA -> do
    v0 <- load A
    v <- Complement v0
    save A v
    return Next
  CMC -> do -- untested
    bit <- GetFlag Cpu.CF
    bit' <- Flip bit
    SetFlag Cpu.CF bit'
    return Next
  MOV {dest,src} -> do
    b <- load src
    save dest b
    return Next
  HLT -> do
    SetHalted
    return Next
  ADD reg -> do
    v1 <- load reg
    cin <- MakeBit False
    addToAccWithCarry cin v1
  ADC reg -> do
    v1 <- load reg
    cin <- GetFlag Cpu.CF
    addToAccWithCarry cin v1
  SUB reg -> do
    v1 <- load reg
    cin <- MakeBit False
    subToAccWithCarry cin v1
  SBB reg -> do
    v1 <- load reg
    cin <- GetFlag Cpu.CF
    subToAccWithCarry cin v1
  ANA reg -> do
    v1 <- load reg
    andA v1
  XRA reg -> do
    v1 <- load reg
    binop XorB v1
  ORA reg -> do
    v1 <- load reg
    binop OrB v1
  CMP reg -> do
    v1 <- load A
    v2 <- load reg
    (v,borrow) <- subtract v1 v2
    setFlagsFrom v
    SetFlag Cpu.CF borrow
    return Next
  RCond cond -> do
    executeCond cond >>= CaseBit >>= \case
      False -> return Next
      True -> do
        lo <- popStack
        hi <- popStack
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump dest)
  POP rp -> do
    lo <- popStack
    hi <- popStack
    case expandRegPair rp of
      Right rr -> do
        a <- MakeAddr $ HiLo{hi,lo}
        SetReg16 rr a
      Left (HiLo{hi=rh, lo=rl}) -> do
        setRegOrFlags rh hi
        setRegOrFlags rl lo
    return Next
  XTHL -> do
    sp0 <- load16 SP
    sp1 <- OffsetAddr 1 sp0
    stack0 <- ReadMem sp0
    stack1 <- ReadMem sp1
    l <- load L
    h <- load H
    WriteMem sp0 l
    WriteMem sp1 h
    save L stack0
    save H stack1
    return Next
  DI -> do
    DisableInterrupts
    return Next
  PUSH rp -> do
    HiLo{hi,lo} <-
      case expandRegPair rp of
        Right rr -> do
          a <- GetReg16 rr
          SplitAddr a
        Left (HiLo{hi=rh, lo=rl}) -> do
          hi <- getRegOrFlags rh
          lo <- getRegOrFlags rl
          pure $ HiLo{hi,lo}
    pushStack hi
    pushStack lo
    return Next
  RST w -> do
    GetReg Cpu.PCH >>= pushStack
    GetReg Cpu.PCL >>= pushStack
    hi <- MakeByte 0
    lo <- MakeByte (8*w)
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  RET -> do
    lo <- popStack
    hi <- popStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  {-RETx{} -> do
    lo <- popStack
    hi <- popStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)-}
  PCHL -> do
    dest <- load16 HL
    return (Jump dest)
  SPHL -> do
    word <- load16 HL
    save16 SP word
    return Next
  XCHG -> do  -- maybe use load16/save16? (if DE becomes a reg16)
    d <- load D
    e <- load E
    h <- load H
    l <- load L
    save D h
    save E l
    save H d
    save L e
    return Next
  EI -> do
    EnableInterrupts
    return Next
  IM2 -> do
    SetInterruptMode 2
    return Next


decimalAdjust :: Bit p -> Bit p -> Byte p-> Eff p (Byte p, Bit p, Bit p)
decimalAdjust auxIn cin byteIn = do

  mask <- MakeByte 0xF
  lo <- AndB byteIn mask
  loMoreThan9 <- nibbleAbove9 lo
  loNeedsAdjust <- OrBit loMoreThan9 auxIn
  loPlus6 <- add6 lo
  loAdjust <- Ite loNeedsAdjust loPlus6 lo
  auxOut <- TestBit loAdjust 4

  four <- MakeByte 0x4
  hi0 <- byteIn `ShiftRight` four
  hi <- incrementMaybe auxOut hi0

  hiMoreThan9 <- nibbleAbove9 hi
  hiNeedsAdjust <- OrBit hiMoreThan9 cin
  hiPlus6 <- add6 hi
  hiAdjust <- Ite hiNeedsAdjust hiPlus6 hi

  cout0 <- TestBit hiAdjust 4
  cout <- OrBit cout0 cin

  lo' <- AndB loAdjust mask
  hi' <- hiAdjust `ShiftLeft` four
  byteOut <- OrB hi' lo'

  return (byteOut,auxOut,cout)


nibbleAbove9 :: Byte p -> Eff p (Bit p)
nibbleAbove9 n = do
  n1 <- n `TestBit` 1 -- 2
  n2 <- n `TestBit` 2 -- 4
  n3 <- n `TestBit` 3 -- 8
  n12 <- OrBit n1 n2
  AndBit n12 n3

incrementMaybe :: Bit p -> Byte p -> Eff p (Byte p)
incrementMaybe cin v = do
  zero <- MakeByte 0x0
  (v',_) <- AddWithCarry cin v zero
  return v'

add6 :: Byte p -> Eff p (Byte p)
add6 v = do
  false <- MakeBit False
  six <- MakeByte 0x6
  (vPlus6,_) <- AddWithCarry false v six
  return vPlus6


executeCond :: Condition -> Eff p (Bit p)
executeCond = \case
  NZ -> GetFlag Cpu.ZF >>= Flip
  Z -> GetFlag Cpu.ZF
  NC -> GetFlag Cpu.CF >>= Flip
  CY -> GetFlag Cpu.CF
  PO -> GetFlag Cpu.PF >>= Flip
  PE -> GetFlag Cpu.PF
  P -> GetFlag Cpu.SF >>= Flip
  MI -> GetFlag Cpu.SF


load :: RegSpec -> Eff p (Byte p)
load = \case
  A -> GetReg Cpu.A
  B -> GetReg Cpu.B
  C -> GetReg Cpu.C
  D -> GetReg Cpu.D
  E -> GetReg Cpu.E
  H -> load16hi HL
  L -> load16lo HL
  M -> load16 HL >>= ReadMem
  I -> GetReg Cpu.I

load16hi :: RegPairSpec -> Eff p (Byte p)
load16hi rr = do
  a <- load16 rr
  HiLo{hi} <- SplitAddr a
  pure hi

load16lo :: RegPairSpec -> Eff p (Byte p)
load16lo rr = do
  a <- load16 rr
  HiLo{lo} <- SplitAddr a
  pure lo

save :: RegSpec -> Byte p -> Eff p ()
save = \case
  A -> SetReg Cpu.A
  B -> SetReg Cpu.B
  C -> SetReg Cpu.C
  D -> SetReg Cpu.D
  E -> SetReg Cpu.E
  H -> save16hi HL
  L -> save16lo HL
  M -> \b -> do a <- load16 HL; WriteMem a b
  I -> SetReg Cpu.I

execute1 :: Op1 -> Byte p -> Eff p (Flow p)
execute1 op1 b1 = case op1 of
  MVI dest -> do
    save dest b1
    return Next
  OUT -> do
    value <- load A
    PortOutput b1 value
    return Next
  ADI -> do
    cin <- MakeBit False
    addToAccWithCarry cin b1
  SUI -> do
    cin <- MakeBit False
    subToAccWithCarry cin b1
  ANI ->
    andA b1
  ORI ->
    binop OrB b1
  IN -> do
    undefined
    {-value <- Ports.inputPort b1
    save A value
    return Next-}
  ACI -> do
    cin <- GetFlag Cpu.CF
    addToAccWithCarry cin b1
  SBI -> do
    cin <- GetFlag Cpu.CF
    subToAccWithCarry cin b1
  XRI -> do
    binop XorB b1
  CPI -> do
    b <- load A
    (v,borrow) <- subtract b b1
    setFlagsFrom v
    SetFlag Cpu.CF borrow
    return Next
  DJNZ -> do
    b <- load B
    cin <- MakeBit False
    ff <- MakeByte 0xFF
    (b',_coutIgnored) <- AddWithCarry cin b ff
    save B b'
    IsZero b' >>= Flip >>= CaseBit >>= \case
      False -> return Next
      True -> jumpRelative b1
  JR cond -> do
    executeCond cond >>= CaseBit >>= \case
      False -> return Next
      True -> jumpRelative b1

jumpRelative :: Byte p -> Eff p (Flow p)
jumpRelative b1 = do
  pc <- getPC
  zero <- MakeByte 0x0
  displacement <- MakeAddr (HiLo{hi=zero,lo=b1})
  (dest,_coutIgnored) <- Add16 pc displacement
  dest' <- OffsetAddr (-256) dest -- TODO: dont do this for forward jumps
  return (Jump dest')

binop
  :: (Byte p -> Byte p -> Eff p (Byte p))
  -> Byte p
  -> Eff p (Flow p)
binop f b1 = do
  v0 <- load A
  v <- f b1 v0
  p <- IsParity v
  SetFlag Cpu.PF p
  saveAndSetFlagsFrom A v
  resetCarry
  resetAux
  return Next

andA
  :: Byte p
  -> Eff p (Flow p)
andA b1 = do
  v0 <- load A
  v <- AndB b1 v0
  p <- IsParity v
  SetFlag Cpu.PF p
  saveAndSetFlagsFrom A v
  resetCarry
  w <- OrB b1 v0
  aux <- TestBit w 3
  SetFlag Cpu.HF aux
  return Next

resetCarry :: Eff p ()
resetCarry = do
  c <- MakeBit False
  SetFlag Cpu.CF c

resetAux :: Eff p ()
resetAux = do
  a <- MakeBit False
  SetFlag Cpu.HF a


execute2 :: Op2 -> Addr p -> Eff p (Flow p)
execute2 op2 a = case op2 of
  LXI rp -> do
    save16 rp a
    return Next
  SHLD -> do
    b <- load L
    WriteMem a b
    a' <- OffsetAddr 1 a
    b' <- load H
    WriteMem a' b'
    return Next
  STA -> do
    b  <- load A
    WriteMem a b
    return Next
  LHLD -> do
    b <- ReadMem a
    save L b
    a' <- OffsetAddr 1 a
    b' <- ReadMem a'
    save H b'
    return Next
  LDA -> do
    b <- ReadMem a
    save A b
    return Next
  JCond cond -> do
    executeCond cond >>= CaseBit >>= \case
      False -> return Next
      True -> return (Jump a)
  JMP -> do
    return (Jump a)
  {-JMPx -> do
    return (Jump a)-}
  CCond cond -> do
    executeCond cond >>= CaseBit >>= \case
      False -> return Next
      True -> do
        pushReturn
        return (Jump a)
  CALL -> do
    pushReturn
    return (Jump a)
  {-CALLx{} ->
    call a-}


addToAccWithCarry :: Bit p -> Byte p -> Eff p (Flow p)
addToAccWithCarry cin v1 = do
  v2 <- load A
  (v,cout) <- AddWithCarry cin v1 v2
  aux <- carryBit 4 cin v1 v2
  SetFlag Cpu.HF aux
  SetFlag Cpu.CF cout
  o <- overflows cin v1 v2
  SetFlag Cpu.PF o
  saveAndSetFlagsFrom A v
  return Next


subToAccWithCarry :: Bit p -> Byte p -> Eff p (Flow p)
subToAccWithCarry cin v2 = do
  v1 <- load A
  (v,cout) <- subWithCarry cin v1 v2
  SetFlag Cpu.CF cout
  o <- overflows cin v1 v2
  SetFlag Cpu.PF o
  saveAndSetFlagsFrom A v
  return Next


subtract :: Byte p -> Byte p -> Eff p (Byte p, Bit p)
subtract v1 v2 = do
  cin <- MakeBit False
  subWithCarry cin v1 v2

subWithCarry :: Bit p -> Byte p -> Byte p -> Eff p (Byte p, Bit p)
subWithCarry cin v1 v2 = do
  cin' <- Flip cin
  v2comp <- Complement v2
  (v,cout) <- AddWithCarry cin' v1 v2comp -- TODO: abstract/share add(with flags)
  aux <- carryBit 4 cin' v1 v2comp
  SetFlag Cpu.HF aux
  o <- overflows cin' v1 v2comp
  SetFlag Cpu.PF o
  borrow <- Flip cout
  return (v, borrow)

overflows :: Bit p -> Byte p -> Byte p -> Eff p (Bit p)
overflows cin v1 v2 = do
  c7 <- carryBit 7 cin v1 v2
  c8 <- carryBit 8 cin v1 v2
  xOrBit c7 c8

xOrBit :: Bit p -> Bit p -> Eff p (Bit p)
xOrBit x y = do
  o <- OrBit x y
  a <- AndBit x y
  n <- Flip a
  AndBit o n

carryBit :: Int -> Bit p -> Byte p -> Byte p -> Eff p (Bit p)
carryBit n cin v1 v2 = do
  (result,_coutIgnored) <- AddWithCarry cin v1 v2
  diffs <- XorB v1 v2
  carry <- XorB result diffs
  if n>8 then error "carryBit" else
    if n==8 then pure _coutIgnored else
      carry `TestBit` n

pushReturn :: Eff p ()
pushReturn = do
  hi <- GetReg Cpu.PCH
  lo <- GetReg Cpu.PCL
  pushStack hi
  pushStack lo
  returnAddr <- MakeAddr $ HiLo{hi,lo}
  MarkReturnAddress returnAddr -- for reachabiity



saveAndSetFlagsFrom :: RegSpec -> Byte p -> Eff p ()
saveAndSetFlagsFrom reg v = do
  save reg v
  setFlagsFrom v

setFlagsFrom :: Byte p -> Eff p ()
setFlagsFrom value = do
  s <- IsSigned value
  z <- IsZero value
  x <- TestBit value 3
  y <- TestBit value 5
  SetFlag Cpu.SF s
  SetFlag Cpu.ZF z
  SetFlag Cpu.XF x
  SetFlag Cpu.YF y
  false <- MakeBit False -- TODO: set NF correctly
  SetFlag Cpu.NF false


pushStack :: Byte p -> Eff p ()
pushStack b = do
  sp0 <- load16 SP
  sp1 <- OffsetAddr (-1) sp0
  save16 SP sp1
  WriteMem sp1 b

popStack :: Eff p (Byte p)
popStack = do
  sp0 <- load16 SP
  sp1 <- OffsetAddr 1 sp0
  save16 SP sp1
  ReadMem sp0

getPC :: Eff p (Addr p)
getPC = do
  hi <- GetReg Cpu.PCH
  lo <- GetReg Cpu.PCL
  MakeAddr $ HiLo{hi,lo}

setPC :: Addr p -> Eff p ()
setPC a = do
  HiLo{hi,lo} <- SplitAddr a
  SetReg Cpu.PCL lo
  SetReg Cpu.PCH hi

load16 :: RegPairSpec -> Eff p (Addr p)
load16 rp = do
  case expandRegPair rp of
    Right rr -> GetReg16 rr
    Left (HiLo{hi=rh, lo=rl}) -> do
      hi <- GetReg rh
      lo <- GetReg rl
      MakeAddr $ HiLo{hi,lo}

save16 :: RegPairSpec -> Addr p -> Eff p ()
save16 rp a = do
  case expandRegPair rp of
    Right rr -> SetReg16 rr a
    Left (HiLo{hi=rh, lo=rl}) -> do
      HiLo{hi,lo} <- SplitAddr a
      SetReg rh hi
      SetReg rl lo

save16hi :: RegPairSpec -> Byte p -> Eff p ()
save16hi rp hi = do
  case expandRegPair rp of
    Right rr -> do
      a <- GetReg16 rr
      HiLo{lo} <- SplitAddr a
      a' <- MakeAddr $ HiLo{hi,lo}
      SetReg16 rr a'
    Left (HiLo{hi=rh}) -> do
      SetReg rh hi

save16lo :: RegPairSpec -> Byte p -> Eff p ()
save16lo rp lo = do
  case expandRegPair rp of
    Right rr -> do
      a <- GetReg16 rr
      HiLo{hi} <- SplitAddr a
      a' <- MakeAddr $ HiLo{hi,lo}
      SetReg16 rr a'
    Left (HiLo{lo=rl}) -> do
      SetReg rl lo


expandRegPair :: RegPairSpec -> Either (HiLo Reg) Reg16
expandRegPair = \case
  BC -> Left $ HiLo {hi = Cpu.B, lo = Cpu.C}
  DE -> Left $ HiLo {hi = Cpu.D, lo = Cpu.E}
  HL -> Right Cpu.HL
  SP -> Right Cpu.SP
  PSW -> Left $ HiLo {hi = Cpu.A, lo = Cpu.F}

setRegOrFlags :: Reg -> Byte p -> Eff p ()
setRegOrFlags r v = case r of
  {-Cpu.F -> do
    TestBit v 7 >>= SetFlag Cpu.SF
    TestBit v 6 >>= SetFlag Cpu.ZF
    TestBit v 4 >>= SetFlag Cpu.HF
    TestBit v 2 >>= SetFlag Cpu.PF
    TestBit v 0 >>= SetFlag Cpu.CF-}
  reg ->
    SetReg reg v

getRegOrFlags :: Reg -> Eff p (Byte p)
getRegOrFlags = \case
  {-Cpu.F -> do
    x <- MakeByte 0x2
    x <- GetFlag Cpu.SF >>= UpdateBit x 7
    x <- GetFlag Cpu.ZF >>= UpdateBit x 6
    x <- GetFlag Cpu.HF >>= UpdateBit x 4
    x <- GetFlag Cpu.PF >>= UpdateBit x 2
    x <- GetFlag Cpu.CF >>= UpdateBit x 0
    return x-}
  reg ->
    GetReg reg
