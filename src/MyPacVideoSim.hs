
module MyPacVideoSim (
  seeCols,
  ) where

import Video (Eff(..),Phase(..),RGB(..))

seeCols :: Eff p ()
seeCols = do
  rgbs <- sequence [ readAndDecode a | a <- [0..15] ]
  sequence_ [seeColour i rgb | (i,rgb) <- zip [0..] rgbs ]
  where
    readAndDecode a = do
      b <- readColByte a
      decodeAsRGB b

readColByte :: Int -> Eff p (Number p)
readColByte a = do
  LitI a >>= ReadColRom

seeColour :: Int -> RGB (Number p) -> Eff p ()
seeColour i rgb = do
  size <- LitI 12
  i_size <- LitI i >>= Mul size
  ten <- LitI 10
  x <- Add i_size ten
  let y = ten
  square (12-2) x y rgb

square :: Int -> Number p -> Number p -> RGB (Number p) -> Eff p ()
square size x0 y0 rgb = do
  forEff (0,size-1) $ \x1 -> do
    forEff (0,size-1) $ \y1 -> do
      x <- Add x0 x1
      y <- Add y0 y1
      SetPixel x y rgb

-- TODO: make this a primitive effect; allowing loops to be constructed
forEff :: (Int,Int) -> ((Number p) -> Eff p ()) -> Eff p ()
forEff (low,high) f =
  sequence_ [ do n <- LitI i; f n | i <- [low..high]]

decodeAsRGB :: Number p ->  Eff p (RGB (Number p))
decodeAsRGB w = do
  let
    bit i v = do
      c <- w `PickBit` i
      CaseBit c >>= \case
        True -> LitI v
        False -> LitI 0

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
    z <- LitI 0
    add3 x y z

  pure RGB { r, g, b }

  where
    add3 a b c = do
      ab <- Add a b
      Add ab c
