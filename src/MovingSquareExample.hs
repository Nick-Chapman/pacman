module MovingSquareExample (square) where

import System
import Value

data MS = MS
  { xposReg :: Reg Nat
  , enterLastReg :: Reg Bit
  , highReg :: Reg Bit
  }

square :: System
square = do
  DeclareReg1 "last" $ \enterLastReg -> do
  DeclareReg1i "high" B1 $ \highReg -> do
  DeclareReg "xpos" Size {size = 8} $ \xposReg -> do
  let ms = MS {enterLastReg,highReg,xposReg}
  let ss = defaultScreenSpec { sf = 3 }
  FrameEffect ss $ do
    moveSquare 5 ms

moveSquare :: Int -> MS -> Eff ()
moveSquare w MS{xposReg,enterLastReg,highReg}= do

    let moveRight = keyDown KeyX -- move the square to the right
    let beGreen = keyDown KeyShift -- change colour to green while pressed
    let enter = keyDown KeyEnter -- toggle x-position as high/low

    xpos <- GetReg xposReg
    enterLast <- GetReg enterLastReg
    beHigh <- GetReg highReg

    toggleHeight <- posEdge enterLast enter
    SetReg enterLastReg enter

    if_ moveRight $ do
      xposPlus1 <- Plus xpos (eSized 8 1)
      SetReg xposReg xposPlus1

    let hiPos = XY {x = xpos, y = nat8 75}
    let loPos = XY {x = xpos, y = nat8 85}

    let red = RGB { r = nat8 255, g = nat8 0, b = nat8 0 }
    let green = RGB { r = nat8 0, g = nat8 255, b = nat8 0 }

    if_ toggleHeight $
      SetReg highReg (eNot beHigh)

    pos <- switchXY beHigh hiPos loPos
    col <- switchRGB beGreen green red

    setSquare w pos col

if_ :: E Bit -> Eff () -> Eff ()
if_ e then_ = do
  Ite e $ \case
    B0 -> pure ()
    B1 -> then_

setSquare :: Int -> XY (E Nat) -> RGB (E Nat) -> Eff ()
setSquare width loc col = do
  let dels = [ XY{x,y} | x <- map nat8 [0..width-1], y <- map nat8 [0..width-1] ]
  sequence_ [do xy <- addXY loc offset; SetPixel xy col | offset <- dels]

switchXY :: E Bit -> XY (E Nat) -> XY (E Nat) -> Eff (XY (E Nat))
switchXY sel XY{x=x1,y=y1} XY{x=x2,y=y2} = do
  x <- Mux sel YN{ yes = x1, no = x2 }
  y <- Mux sel YN{ yes = y1, no = y2 }
  pure XY{x,y}

switchRGB :: E Bit -> RGB (E Nat) -> RGB (E Nat) -> Eff (RGB (E Nat))
switchRGB sel RGB{r=r1,g=g1,b=b1} RGB{r=r2,g=g2,b=b2} = do
  r <- Mux sel YN{ yes = r1, no = r2 }
  g <- Mux sel YN{ yes = g1, no = g2 }
  b <- Mux sel YN{ yes = b1, no = b2 }
  pure RGB{r,g,b}

addXY :: XY (E Nat) -> XY (E Nat) -> Eff (XY (E Nat))
addXY XY{x=x1,y=y1} XY{x=x2,y=y2} = do
  x <- Plus x1 x2
  y <- Plus y1 y2
  pure $ XY {x,y}

posEdge :: E Bit -> E Bit -> Eff (E Bit)
posEdge x y = And (eNot x) y

nat8 :: Int -> E Nat
nat8 = eSized 8
