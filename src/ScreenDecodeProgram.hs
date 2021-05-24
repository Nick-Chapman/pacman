
-- | System to decode a pacman screen from memory (hand written, following Lomont emulation guide)
-- | The memory is loaded from 2-dump files (loaded as roms)
-- | Code for decoding/drawing sprites and tiles is accessed from DisplayTilesAndSprites
-- | The screen image is composed of tiles and sprites, as specified by the memory dump.

module ScreenDecodeProgram (screenByHand) where

import DisplayTilesAndSprites (
  readPalette, PaletteIndex(..),
  readSprite, SpriteIndex(..), drawSprite,
  readTile, TileIndex(..), drawTile,
  )

import Pacman_RomsAndRams (VideoRoms(..),withVideoRoms,RamDump(..),withRamDump)
import System (System(..),Eff(..),E,eSized,index,combine,split)
import Value (ScreenSpec(..),defaultScreenSpec,XY(..),Nat)

screenByHand :: String -> System
screenByHand suf = do
  withVideoRoms $ \roms -> do
  withRamDump suf $ \dump -> do
    let (x,y) = (224,288) -- (8*28, 8*36)
    let ss = defaultScreenSpec { sf = 3, size = XY { x, y }}
    FrameEffect ss $ do
      drawTiles dump roms
      sequence_ [drawSpriteIndex dump roms i | i <- [0..7]]
      pure ()

drawSpriteIndex :: RamDump -> VideoRoms -> Int -> Eff ()
drawSpriteIndex RamDump{ram,sprite_xy_ram} roms i = do
  info <- ReadRom ram (eSized 12 (0xff0 + 2 * fromIntegral i))
  palb <- ReadRom ram (eSized 12 (0xff1 + 2 * fromIntegral i))
  xLoc <- ReadRom sprite_xy_ram (eSized 4 (0 + 2 * fromIntegral i))
  yLoc <- ReadRom sprite_xy_ram (eSized 4 (1 + 2 * fromIntegral i))
  palette <- readPalette roms (PaletteIndex (mod64 palb))
  let yFlip = info `index` 0
  let xFlip = info `index` 1
  let spriteIndex = div4 info
  sprite <- readSprite roms (SpriteIndex spriteIndex)
  let xMax = eSized 9 (28*8 + 15)
  let yMax = eSized 9 (32*8 + 16)
  x <- Minus xMax xLoc
  y <- Minus yMax yLoc
  let xy = XY { x, y }
  drawSprite (xFlip,yFlip) xy sprite palette

div4 :: E Nat -> E Nat
div4 i = combine (reverse (drop 2 (reverse (split i))))

drawTiles :: RamDump -> VideoRoms -> Eff ()
drawTiles dump roms = do
  let
    width = 28
    height = 32 -- of mid-screen area
    top =
      [ (XY{x = 8*(width-1-i), y = 8*0}, 0x3c2 + i) | i <- [0..width-1] ] ++
      [ (XY{x = 8*(width-1-i), y = 8*1}, 0x3e2 + i) | i <- [0..width-1] ]
    mid =
      [ (XY{x = 8*(width-1-i), y = 8*(2+j)}, 0x40 + j + height*i)
      | i <- [0..width-1], j <- [0..height-1]
      ]
    bot =
      [ (XY{x = 8*(width-1-i), y = 8*34}, 0x02 + i) | i <- [0..width-1] ] ++
      [ (XY{x = 8*(width-1-i), y = 8*35}, 0x22 + i) | i <- [0..width-1] ]
  sequence_
    [ drawSelectedTile dump roms xy i | (xy,i::Int) <- top ++ mid ++ bot ]

drawSelectedTile :: RamDump -> VideoRoms -> XY Int -> Int -> Eff ()
drawSelectedTile RamDump{ram} roms xy i = do
  byteT <- ReadRom ram (eSized 16 (fromIntegral i))
  tile <- readTile roms (TileIndex byteT)
  byteP <- ReadRom ram (eSized 16 (0x400 + fromIntegral i))
  let six = mod64 byteP
  let pi = PaletteIndex six
  palette <- readPalette roms pi
  drawTile (fmap (eSized 9) xy) tile palette

mod64 :: E Nat -> E Nat
mod64 i = combine (reverse (take 6 (reverse (split i))))
