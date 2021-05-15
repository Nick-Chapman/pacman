module SmallExamples (square,cols) where

import System
import Value

cols :: System
cols = do
  DeclareRom (RomSpec { path = "roms/82s123.7f", size = 32 }) $ \colRom -> do
  DeclareReg "rx" (Size 3) $ \rx -> do
  DeclareReg "ry" (Size 3) $ \ry -> do
  DeclareReg "ri" (Size 4) $ \ri -> do
  let rr = (rx,ry)
  let ss = defaultScreenSpec { sf = 4, size = XY { x = 256, y = 20 } }
  FrameEffect ss $ do
    Repeat 64 $ do
      SetReg ri (eSized 4 0)
      Repeat 16 $ do
        colSquare rr colRom ri
        increment ri
      incXY rr

_eRepeat :: Int -> Eff() -> Eff ()
_eRepeat n e = sequence_ (replicate n e)

increment :: Reg Nat -> Eff ()
increment r = do
  n <- GetReg r
  n' <- Plus n (eSized (sizeE n) 1)
  SetReg r n'

colSquare :: (Reg Nat, Reg Nat) -> RomId -> Reg Nat -> Eff ()
colSquare rr colRom ri = do
  i <- GetReg ri
  byte <- ReadRomByte colRom i
  Trace "SmallExample/cols, rom lookup" [i,byte]
  col <- decodeAsRGB byte
  let x = combine (split i ++ [b0,b0,b0,b0])
  let xy = XY { x, y = nat8 0 }
  setSquareR rr xy col
    where b0 = eLit 0 B0

setSquareR :: (Reg Nat, Reg Nat) -> XY (E Nat) -> RGB (E Nat) -> Eff ()
setSquareR (rx,ry) loc col = do
  x <- GetReg rx
  y <- GetReg ry
  let offset = XY{x,y}
  xy <- addXY loc offset
  SetPixel xy col

incXY :: (Reg Nat, Reg Nat) -> Eff ()
incXY (rx,ry) = do
  x <- GetReg rx
  y <- GetReg ry
  x' <- Plus x one
  SetReg rx x'
  carry <- allBitsSet (split x)
  y1 <- Plus y one
  y' <- Mux carry YN{yes = y1, no = y}
  SetReg ry y'

allBitsSet :: [E Bit] -> Eff (E Bit)
allBitsSet = \case
  [] -> pure (eLit 1 B1)
  x:xs -> do
    y <- allBitsSet xs
    And x y

decodeAsRGB :: E Nat -> Eff (RGB (E Nat))
decodeAsRGB w = do
  let
    bit :: Int -> Int -> Eff (E Nat)
    bit i v =
      switch (w `index` i) (nat8 v) (nat8 0)
      --_muxBits (w `index` i) (nat8 v) (nat8 0)
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


data MS = MS
  { xposReg :: Reg Nat
  , enterLastReg :: Reg Bit
  , highReg :: Reg Bit
  }

square :: System
square = do
  DeclareReg1 "last" $ \enterLastReg -> do
  DeclareReg1 "high" $ \highReg -> do
  DeclareReg "xpos" Size {size = 8} $ \xposReg -> do
  let ms = MS {enterLastReg,highReg,xposReg}
  let ss = defaultScreenSpec { sf = 3 }
  FrameEffect ss $ do
    moveSquare 5 ms

moveSquare :: Int -> MS -> Eff ()
moveSquare w MS{xposReg,enterLastReg,highReg}= do

    let goingRight = keyDown KeyX
    let shift = keyDown KeyShift -- colour
    let enter = keyDown KeyEnter -- switch high/low line

    xpos <- GetReg xposReg
    enterLast <- GetReg enterLastReg
    high <- GetReg highReg

    switchHeight <- posEdge enterLast enter
    SetReg enterLastReg enter

    {-xposPlus1 <- Plus xpos one
    nextXpos <- switch goingRight xposPlus1 xpos
    SetReg xposReg nextXpos-}
    if_ goingRight $ do
      xposPlus1 <- Plus xpos one
      SetReg xposReg xposPlus1

    let here = XY {x = xpos, y = nat8 75}
    let there = XY {x = xpos, y = nat8 85}

    let red = RGB { r = nat8 255, g = nat8 0, b = nat8 0 }
    let green = RGB { r = nat8 0, g = nat8 255, b = nat8 0 }

    {-highBar <- notG high
    nextHigh <- mux switchHeight highBar high
    SetReg highReg nextHigh-}
    if_ switchHeight $
      SetReg highReg (eNot high)

    loc <- switchXY high here there
    col <- switchRGB shift green red

    setSquare w loc col


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
  x <- switch sel x1 x2
  y <- switch sel y1 y2
  pure XY{x,y}

switchRGB :: E Bit -> RGB (E Nat) -> RGB (E Nat) -> Eff (RGB (E Nat))
switchRGB sel RGB{r=r1,g=g1,b=b1} RGB{r=r2,g=g2,b=b2} = do
  r <- switch sel r1 r2
  g <- switch sel g1 g2
  b <- switch sel b1 b2
  pure RGB{r,g,b}

{-
-- | very generic on switched type; causes code explosion
_switch :: E Bit -> a -> a -> Eff a
_switch sel yes no = do
  CaseBit sel >>= \case
    B1 -> pure yes
    B0 -> pure no
-}

-- | type is less general, fixed to 'E [Bit]'. Could it be 'E a' ?
switch :: E Bit -> E [Bit] -> E [Bit] -> Eff (E [Bit])
switch sel yes no = do
  Mux sel YN {yes,no}

addXY :: XY (E Nat) -> XY (E Nat) -> Eff (XY (E Nat))
addXY XY{x=x1,y=y1} XY{x=x2,y=y2} = do
  x <- Plus x1 x2
  y <- Plus y1 y2
  pure $ XY {x,y}

_muxBits :: E Bit -> E [Bit] -> E [Bit] -> Eff (E [Bit]) -- muxes individual bits
_muxBits sel yes no = do
  combine <$> sequence [ mux sel y n | (y,n) <- zipChecked (split yes) (split no) ]

zipChecked :: [a] -> [b] -> [(a,b)]
zipChecked xs ys = do
  let xn = length xs
  let yn = length ys
  if xn /= yn then error (show ("zipChecked",xn,yn)) else
    zip xs ys


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
nat8 = eSized 8

--nibble :: Int -> E Nat
--nibble = eSized 4

one :: E [Bit]
one = eSized (Size 1) 1
