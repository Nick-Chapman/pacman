
module MyPacVideoSim (
  seeCols,
  ) where

import Video (Eff(..),Phase(..),RGB(..))
import Data.Word8 (Word8)
import Data.Bits

seeCols :: Eff p ()
seeCols = do
  rgbs <- mapM decodeAsRGB [ a * 16 + a | a <- [0..15] ]
  sequence_ [seeColour i rgb | (i,rgb) <- zip [0..] rgbs ]

seeColour :: Int -> RGB (Byte p) -> Eff p ()
seeColour i rgb = do
  size <- LitI 12
  i_size <- LitI i >>= Mul size
  ten <- LitI 10
  x <- Add i_size ten
  let y = ten
  square (12-2) x y rgb

square :: Int -> Number p -> Number p -> RGB (Byte p) -> Eff p ()
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

decodeAsRGB :: Word8 -> Eff p (RGB (Byte p))
decodeAsRGB w = do
  let bit i v = if w `testBit` i then v else 0
  r <- LitB $ bit 0 0x21 + bit 1 0x47 + bit 2 0x97
  g <- LitB $ bit 3 0x21 + bit 4 0x47 + bit 5 0x97
  b <- LitB $ bit 6 0x51 + bit 7 0xAE
  pure $ RGB { r, g, b }
