
module AnExampleSystem (small) where

import Types (System(..),Eff(..),XY(..),RGB(..),E(..),Nat,Bit(..),Key(..),
              SizeSpec(..),
              ePosInt,eNot)

small :: System
small = do
  DeclareReg1 $ \enterLastReg -> do
  DeclareReg1 $ \highReg -> do
  DeclareReg SizeSpec {size = 7} $ \xposReg -> do
  FrameEffect $ do

    --let z = E_KeyDown KeyZ -- move left
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
nat8 = ePosInt (SizeSpec 8)

_zero :: E Bit
_zero = E_Lit (SizeSpec 1) B0

one :: E [Bit]
--one = E_Lit (SizeSpec 1) B1
one = ePosInt (SizeSpec 1) 1
