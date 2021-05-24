
-- | A system for decoding & displaying pacman roms.
-- | Also provides components which are reused by Pacman_ScreenDecodeProgram

module DisplayTilesAndSprites (
  -- complete system
  system,
  -- used by Pacman_ScreenDecodeProgram
  readPalette, PaletteIndex(..), Palette(..),
  readSprite, SpriteIndex(..), Sprite(..), drawSprite,
  readTile, TileIndex(..), Tile(..), drawTile,
  ) where

import Data.List (transpose)
import Pacman_Roms (VideoRoms(..),withVideoRoms)
import System
import Value

system :: System
system = withVideoRoms $ \roms -> do
  let ss = defaultScreenSpec { sf = 3, size = XY { x = 330, y = 256 } }
  FrameEffect ss $ do
    seeCols roms
    seePals roms
    seeTiles roms
    seeSprites roms

seeCols :: VideoRoms -> Eff ()
seeCols VideoRoms{col_rom_7f} = sequence_
  [ do
      b <- ReadRom col_rom_7f (eSized (Size 4) i)
      col <- decodeAsRGB b
      let xy = XY { x = nat8 (14 * i), y = nat8 0 }
      setSquare 14 xy col
  |
    i <- [0..15]
  ]

seePals :: VideoRoms -> Eff ()
seePals mac = sequence_
  [ do
      let yoff = 30
      let pi = PaletteIndex (eSized 6 i)
      palette <- readPalette mac pi
      let w = 7
      let xy = XY { x = nat8 (w * i), y = nat8 (yoff + w * j) }
      rgb <- resolvePaletteItemIndex palette pii
      setSquare w xy rgb
  |
    i <- [0..31]
  , (j,pii) <- zip [0..3] [PII h l | h <- [b1,b0], l <- [b1,b0]]
  ]

seeTiles :: VideoRoms -> Eff ()
seeTiles mac = do
  let pi = PaletteIndex (eSized 6 1)
  palette <- readPalette mac pi
  sequence_
    [ do
        let xoff = 0
        let yoff = 80
        let i = 16*y + x
        let xy = XY { x = nat8 (xoff + 10*x), y = nat9 (yoff + 10*y) }
        tile <- readTile mac (TileIndex (nat8 i))
        drawTile xy tile palette
    |
      x <- [0..15]
    , y <- [0..15]
    ]

seeSprites :: VideoRoms -> Eff ()
seeSprites mac = do
  let pi = PaletteIndex (eSized 6 1)
  palette <- readPalette mac pi
  sequence_
    [ do
        let xoff = 170
        let yoff = 80
        let i = 8*y + x
        let xy = XY { x = nat9 (xoff + 20*x), y = nat9 (yoff + 20*y) }
        sprite <- readSprite mac (SpriteIndex (nat8 i))
        drawSprite (b0,b0) xy sprite palette
    |
      x <- [0..7]
    , y <- [0..7]
    ]

setSquare :: Int -> XY (E Nat) -> RGB (E Nat) -> Eff ()
setSquare width loc col = do
  let dels = [ XY{x,y} | x <- map nat8 [0..width-1], y <- map nat8 [0..width-1] ]
  sequence_ [do xy <- addXY loc offset; SetPixel xy col | offset <- dels]


drawSprite :: (E Bit, E Bit) -> XY (E Nat) -> Sprite -> Palette -> Eff ()
drawSprite (xFlip,yFlip) xy sprite palette = do
  let (Sprite piis) = sprite
  xys <- sequence [ do
                      x <- Mux xFlip YN { yes = nat8 (15-xd), no = nat8 xd }
                      y <- Mux yFlip YN { yes = nat8 (15-yd), no = nat8 yd }
                      addXY xy (XY { x, y })
                  | yd <- [0..15] , xd <- [0..15]
                  ]
  sequence_ [ do
                rgb <- resolvePaletteItemIndex palette pii
                transparent <- isBlack rgb
                Ite transparent $ \case
                  B1 -> pure ()
                  B0 -> SetPixel xy rgb
            | (xy,pii) <- zip xys piis
            ]

isBlack :: RGB (E Nat) -> Eff (E Bit)
isBlack RGB{r,g,b} = do
  rz <- IsZero r
  gz <- IsZero g
  bz <- IsZero b
  rgz <- And rz gz
  And rgz bz

-- draw a tile selected by index
drawTile :: XY (E Nat) -> Tile -> Palette -> Eff ()
drawTile xy tile palette = do
  let (Tile piis) = tile
  xys <- sequence [ addXY xy (XY { x = nat8 xd, y = nat8 yd })
                  | yd <- [0..7] , xd <- [0..7]
                  ]
  sequence_ [ do
                rgb <- resolvePaletteItemIndex palette pii
                SetPixel xy rgb
            | (xy,pii) <- zip xys piis
            ]

addXY :: XY (E Nat) -> XY (E Nat) -> Eff (XY (E Nat))
addXY XY{x=x1,y=y1} XY{x=x2,y=y2} = do
  x <- Plus x1 x2
  y <- Plus y1 y2
  pure $ XY {x,y}

newtype SpriteIndex = SpriteIndex (E Nat) -- 0..63
data Sprite = Sprite [PaletteItemIndex] -- #256 (16x16) for each sprite pixel

readSprite :: VideoRoms -> SpriteIndex -> Eff Sprite
readSprite VideoRoms{char_rom_5f} (SpriteIndex i) = do
  let
    readStrip :: Int -> Eff [PaletteItemIndex]
    readStrip off = do
      let i64 = times64 i -- 64 bytes per sprite
      a <- Plus i64 (eSized 6 off)
      byte <- ReadRom char_rom_5f a
      pure $ decodeTileByte byte
  layer1 <- mapM readStrip (reverse ([8..8+7] ++ [32+8+0..32+8+7]))
  layer2 <- mapM readStrip (reverse ([16..16+7] ++ [32+16+0..32+16+7]))
  layer3 <- mapM readStrip (reverse ([24..24+7] ++ [32+24+0..32+24+7]))
  layer4 <- mapM readStrip (reverse ([0..7] ++ [32+0..32+7]))
  let piis = concat [ concat (transpose layer) | layer <- [layer1,layer2,layer3,layer4] ]
  pure $ Sprite piis

times64 :: E Nat -> E Nat
times64 i = combine (split i ++ [b0,b0,b0,b0,b0,b0])

newtype TileIndex = TileIndex (E Nat) -- 0..255
data Tile = Tile [PaletteItemIndex] -- #64 (8x8) for each tile pixel

data PaletteItemIndex = PII { h :: E Bit, l :: E Bit }

resolvePaletteItemIndex :: Palette -> PaletteItemIndex -> Eff (RGB (E Nat))
resolvePaletteItemIndex Palette{p0,p1,p2,p3} PII {h,l} = do
  -- 4-way mux
  p32 <- muxRGB l p3 p2
  p10 <- muxRGB l p1 p0
  muxRGB h p32 p10

muxRGB :: E Bit -> RGB (E Nat) -> RGB (E Nat) -> Eff (RGB (E Nat))
muxRGB sel RGB{r=yr,g=yg,b=yb} RGB{r=nr,g=ng,b=nb} = do
  r <- Mux sel YN {yes = yr, no = nr}
  g <- Mux sel YN {yes = yg, no = ng}
  b <- Mux sel YN {yes = yb, no = nb}
  pure $ RGB { r, g, b }

readTile :: VideoRoms -> TileIndex -> Eff Tile
readTile VideoRoms{char_rom_5e} (TileIndex i) = do
  let
    readStrip off = do
      let i16 = times16 i -- 16 bytes per tile
      a <- Plus i16 (eSized 4 off)
      byte <- ReadRom char_rom_5e a
      pure $ decodeTileByte byte
  bot <- mapM readStrip (reverse [0..7])
  top <- mapM readStrip (reverse [8..15])
  let piis = concat (transpose top) ++ concat (transpose bot)
  pure $ Tile piis

times16 :: E Nat -> E Nat
times16 i = combine (split i ++ [b0,b0,b0,b0])

decodeTileByte :: E Nat -> [PaletteItemIndex] -- #4
decodeTileByte w = do
  case split w of
    [a,b,c,d,e,f,g,h] -> [ PII a e , PII b f , PII c g , PII d h ]
    _ ->
      error "decodeTileByte, expected 8 bits"

newtype PaletteIndex = PaletteIndex (E Nat) -- 0..63 (6 bits of the rom byte)

data Palette = Palette
  { p0 :: RGB (E Nat)
  , p1 :: RGB (E Nat)
  , p2 :: RGB (E Nat)
  , p3 :: RGB (E Nat)
  }

readPalette :: VideoRoms -> PaletteIndex -> Eff Palette
readPalette roms@VideoRoms{col_rom_4a} (PaletteIndex i) = do
  p0 <- readItem 0
  p1 <- readItem 1
  p2 <- readItem 2
  p3 <- readItem 3
  pure $ Palette { p0, p1, p2, p3 }
  where
    readItem :: Int -> Eff (RGB (E Nat))
    readItem off = do
      let i4 = times4 i
      a <- Plus i4 (eSized 2 off)
      byte <- ReadRom col_rom_4a a
      let ci = CI byte
      readColour roms ci

times4 :: E Nat -> E Nat
times4 i = do combine (split i ++ [b0,b0])

newtype ColourIndex = CI (E Nat)

readColour :: VideoRoms -> ColourIndex -> Eff (RGB (E Nat))
readColour VideoRoms{col_rom_7f} (CI i) = do
  byte <- ReadRom col_rom_7f i
  decodeAsRGB byte

decodeAsRGB :: E Nat -> Eff (RGB (E Nat))
decodeAsRGB w = do
  let
    bit :: Int -> Int -> Eff (E Nat)
    bit i v = do
      let c = w `index` i
      Mux c YN { yes = nat8 v, no = nat8 0 }
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

nat8,nat9 :: Int -> E Nat
nat8 = eSized 8
nat9 = eSized 9

b0,b1 :: E Bit
b0 = eLit 1 B0
b1 = eLit 1 B1
