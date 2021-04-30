
module AnExampleSystem (small) where

import Types (System(..),Eff(..),XY(..),RGB(..),E(..),Nat,Bit(..),Key(..),
              ePosInt,eNot)

small :: System
small = do
  DeclareReg1 $ \zLastReg -> do
  DeclareReg1 $ \movedReg -> do
  FrameEffect $ do

    let here = XY {x = nat 50, y = nat 75}
    let there = XY {x = nat 70, y = nat 85}

    let red = RGB { r = nat 255, g = nat 0, b = nat 0 }
    let green = RGB { r = nat 0, g = nat 255, b = nat 0 }

    let x = E_KeyDown KeyX
    let z = E_KeyDown KeyZ

    zLast <- GetReg zLastReg
    makeAmove <- posEdge zLast z
    SetReg zLastReg z

    moved <- GetReg movedReg
    movedBar <- notG moved
    nextMoved <- mux makeAmove movedBar moved
    SetReg movedReg nextMoved

    loc <- CaseBit moved >>= \case B0 -> pure here; B1 -> pure there
    col <- CaseBit x >>= \case B0 -> pure red; B1 -> pure green

    SetPixel loc col


mux :: E Bit -> E Bit -> E Bit -> Eff (E Bit)
mux sel x y = do
  selb <- notG sel
  a <- andG sel x
  b <- andG selb y
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
--andG x y = CaseBit x >>= \b -> pure $ case b of B1 -> y; B0 -> E_Lit B0 -- TOOD: use to test const folding

nat :: Int -> E Nat
nat = ePosInt
