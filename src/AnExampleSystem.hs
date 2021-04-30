
module AnExampleSystem (small) where

import Types --(System(..),Eff(..),XY(..),RGB(..),E(..),Nat,Key(..),UpDown(..),ePosInt)

small :: System
small = do
  DeclareReg1 $ \zLastReg -> do
  DeclareReg1 $ \movedReg -> do
  FrameEffect $ do

    let here = XY {x = nat 50, y = nat 75}
    let there = XY {x = nat 70, y = nat 85}

    z <- KeyDown KeyZ

    zLast <- GetReg zLastReg
    makeAmove <- posEdge zLast z

    moved <- GetReg movedReg
    loc <- CaseBit moved >>= \case B0 -> pure here; B1 -> pure there

    --enter <- KeyDown KeyEnter
    --bv <- CaseBit enter >>= \case B0 -> pure 0; B1 -> pure 255

    let red = RGB { r = nat 255, g = nat 0, b = nat 0 }
    let green = RGB { r = nat 0, g = nat 255, b = nat 0 }

    x <- KeyDown KeyX
    col <- CaseBit x >>= \case B0 -> pure red; B1 -> pure green

    SetPixel loc col

    SetReg zLastReg z

    movedBar <- Not moved

    nextMoved <- mux makeAmove movedBar moved
    SetReg movedReg nextMoved

mux :: E Bit -> E Bit -> E Bit -> Eff (E Bit)
mux sel x y = do
  selb <- Not sel
  a <- andG sel x
  b <- andG selb y
  orG a b

orG :: E Bit -> E Bit -> Eff (E Bit)
orG x y = do
  xb <- Not x
  yb <- Not y
  w <- andG xb yb
  Not w

posEdge :: E Bit -> E Bit -> Eff (E Bit)
posEdge x y = do
  xbar <- Not x
  andG xbar y

andG :: E Bit -> E Bit -> Eff (E Bit)
andG = And -- TODO, to avoid another CaseBit
--andG x y = CaseBit x >>= \b -> pure $ case b of B1 -> y; B0 -> E_Lit B0

nat :: Int -> E Nat
nat = ePosInt
