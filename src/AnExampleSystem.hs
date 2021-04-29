
module AnExampleSystem (small) where

import Types (System(..),Eff(..),XY(..),RGB(..),E(..),Nat,Key(..),UpDown(..),ePosInt)

small :: System
small = do
  FrameEffect $ do

    let here = XY {x = nat 50, y = nat 75}
    let there = XY {x = nat 70, y = nat 85}
    z <- GetKey KeyZ
    loc <- case z of Up -> pure here; Down -> pure there

    enter <- GetKey KeyEnter
    let bv = case enter of Up -> 0; Down -> 255

    let red = RGB { r = nat 255, g = nat 0, b = nat bv }
    let green = RGB { r = nat 0, g = nat 255, b = nat bv }
    x <- GetKey KeyX
    col <- case x of Up -> pure red; Down -> pure green

    SetPixel loc col

nat :: Int -> E Nat
nat = ePosInt
