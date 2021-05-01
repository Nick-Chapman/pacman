
module SmallExamples (combined) where

import Types (System(..),Eff(..),XY(..),RGB(..),E(..),Nat,Bit(..),Key(..),
              Size(..), RomSpec(..), RomId(..),
              bitsOfInt,
              index,
              eNot)

colSquare :: RomId -> Int -> Eff ()
colSquare colRom i = do
  let nib = nibble i
  byte <- ReadRomByte colRom nib
  col <- decodeAsRGB byte
  let xy = XY { x = nat8 (14 * i), y = nat8 0 }
  setSquare 5 xy col

decodeAsRGB :: E Nat -> Eff (RGB (E Nat))
decodeAsRGB w = do
  let
    bit :: Int -> Int -> Eff (E Nat)
    bit i v = do
      c <- pure w `index` i
      muxBits c (lit8 v) (lit8 0)
  r <- do
    x <- bit 0 0x21
    y <- bit 1 0x47
    z <- bit 2 0x97
    add3 x y z
  g <- do
    x <- bit 3 0x21
    y <- bit 4 0x47
    z <- bit 5 0x97
    add3 x y z
  b <- do
    x <- bit 6 0x51
    y <- bit 7 0xAE
    let z = nat8 0
    add3 x y z
  pure RGB { r, g, b }
  where add3 a b c = do ab <- Plus a b; Plus ab c


combined :: System
combined = do
  DeclareReg1 $ \enterLastReg -> do
  DeclareReg1 $ \highReg -> do
  DeclareReg Size {size = 7} $ \xposReg -> do
  DeclareRom (RomSpec { path = "roms/82s123.7f", size = 32 }) $ \colRom -> do
  FrameEffect $ do

    sequence_ [colSquare colRom i | i <- [0..15]]

    let goingRight = E_KeyDown KeyX
    let shift = E_KeyDown KeyShift -- colour
    let enter = E_KeyDown KeyEnter -- switch high/low line

    xpos <- GetReg xposReg

    enterLast <- GetReg enterLastReg
    switchHeight <- posEdge enterLast enter
    SetReg enterLastReg enter

    xposPlus1 <- Plus xpos one
    nextXpos <- switch goingRight xposPlus1 xpos
    SetReg xposReg nextXpos

    let here = XY {x = xpos, y = nat8 75}
    let there = XY {x = xpos, y = nat8 85}

    let red = RGB { r = nat8 255, g = nat8 0, b = nat8 0 }
    let green = RGB { r = nat8 0, g = nat8 255, b = nat8 0 }

    high <- GetReg highReg
    highBar <- notG high
    nextHigh <- mux switchHeight highBar high
    SetReg highReg nextHigh

    loc <- switch high here there
    col <- switch shift green red

    setSquare 5 loc col

setSquare :: Int -> XY (E Nat) -> RGB (E Nat) -> Eff ()
setSquare width loc col = do
  let dels = [ XY{x,y} | x <- map nat8 [0..width-1], y <- map nat8 [0..width-1] ]
  sequence_ [do xy <- addXY loc offset; SetPixel xy col | offset <- dels]

addXY :: XY (E Nat) -> XY (E Nat) -> Eff (XY (E Nat))
addXY XY{x=x1,y=y1} XY{x=x2,y=y2} = do
  x <- Plus x1 x2
  y <- Plus y1 y2
  pure $ XY {x,y}

-- TODO: move all this stuff out to a Lib.hs

muxBits :: E Bit -> Eff (E [Bit]) -> Eff (E [Bit]) -> Eff (E [Bit])
muxBits sel yes no = do
  ys <- Split yes
  ns <- Split no
  sequence [ mux sel y n | (y,n) <- zipChecked ys ns ] >>= Combine

zipChecked :: [a] -> [b] -> [(a,b)]
zipChecked xs ys = do
  let xn = length xs
  let yn = length ys
  if xn /= yn then error (show ("zipChecked",xn,yn)) else
    zip xs ys

-- | generic on switched type; cause code explosion
switch :: E Bit -> a -> a -> Eff a
switch sel yes no = do
  CaseBit sel >>= \case
    B1 -> pure yes
    B0 -> pure no

-- | switched type must be E Bit
mux :: E Bit -> E Bit -> E Bit -> Eff (E Bit)
mux sel yes no = do
  selb <- notG sel
  a <- andG sel yes
  b <- andG selb no
  orG a b

orG :: E Bit -> E Bit -> Eff (E Bit)
orG x y = do
  xb <- notG x
  yb <- notG y
  w <- andG xb yb
  notG w

notG :: E Bit -> Eff (E Bit)
notG = pure . eNot

posEdge :: E Bit -> E Bit -> Eff (E Bit)
posEdge x y = do
  xbar <- notG x
  andG xbar y

andG :: E Bit -> E Bit -> Eff (E Bit)
andG = And
--andG x y = switch x y _zero -- explode/test-const prop

nat8 :: Int -> E Nat
nat8 = ePosInt (Size 8)

nibble :: Int -> E Nat
nibble = ePosInt (Size 4)

one :: E [Bit]
one = ePosInt (Size 1) 1

ePosInt :: Size -> Int -> E Nat
ePosInt size i = do
  let bits = bitsOfInt size i
  E_Lit Size { size = length bits } bits


lit8 :: Int -> Eff (E Nat)
lit8 i = do
  let size = 8
  let bits = bitsOfInt size i
  LitV bits
