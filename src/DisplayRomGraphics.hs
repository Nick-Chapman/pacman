
module DisplayRomGraphics (tiles,screen) where

import Data.List (transpose)
import System
import Value

tiles :: System
tiles = withMac "dump" $ \mac -> do
  let ss = defaultScreenSpec { sf = 3, size = XY { x = 330, y = 256 } }
  FrameEffect ss $ do
    seeCols mac
    seePals mac
    seeTiles mac
    seeSprites mac

screen :: String -> System
screen suf = withMac suf $ \mac -> do
  let (x,y) = (224,288) -- (8*28, 8*36)
  let ss = defaultScreenSpec { sf = 3, size = XY { x, y }}
  FrameEffect ss $ do
    drawTiles mac
    sequence_ [drawSpriteIndex mac i | i <- [0..7]]
    pure ()

withMac :: String -> (Mac -> System) -> System
withMac suf f =
  DeclareRom (RomSpec { path = "roms/82s123.7f", size = 32 }) $ \colRom -> do
  DeclareRom (RomSpec { path = "roms/82s126.4a", size = 256 }) $ \palRom -> do
  DeclareRom (RomSpec { path = "roms/pacman.5e", size = 4096 }) $ \tileRom -> do
  DeclareRom (RomSpec { path = "roms/pacman.5f", size = 4096 }) $ \spriteRom -> do
  DeclareRom (RomSpec { path = "dumps/ram."++suf, size = 4096 }) $ \ramDump -> do
  DeclareRom (RomSpec { path = "dumps/xy."++suf, size = 16 }) $ \xyDump -> do
  f (Mac { colRom, palRom, tileRom, spriteRom, ramDump, xyDump})

data Mac = Mac
  { colRom :: RomId
  , palRom :: RomId
  , tileRom :: RomId
  , spriteRom :: RomId
  , ramDump :: RomId
  , xyDump :: RomId
  }

----------------------------------------------------------------------
-- debugging: see cols,palettes,tiles

seeCols :: Mac -> Eff ()
seeCols Mac{colRom} = sequence_
  [ do
      b <- ReadRom colRom (eSized (Size 4) i)
      col <- decodeAsRGB b
      let xy = XY { x = nat8 (14 * i), y = nat8 0 }
      setSquare 14 xy col
  |
    i <- [0..15]
  ]

seePals :: Mac -> Eff ()
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

seeTiles :: Mac -> Eff ()
seeTiles mac = do
  let pi = PaletteIndex (eSized 6 1)
  palette <- readPalette mac pi
  sequence_
    [ do
        let xoff = 0
        let yoff = 80
        let i = 16*y + x
        let xy = XY { x = nat8 (xoff + 10*x), y = nat9 (yoff + 10*y) }
        tile <- readTile mac (TI (nat8 i))
        drawTile1 xy tile palette
    |
      x <- [0..15]
    , y <- [0..15]
    ]

seeSprites :: Mac -> Eff ()
seeSprites mac = do
  let pi = PaletteIndex (eSized 6 1)
  palette <- readPalette mac pi
  sequence_
    [ do
        let xoff = 170
        let yoff = 80
        let i = 8*y + x
        let xy = XY { x = nat9 (xoff + 20*x), y = nat9 (yoff + 20*y) }
        sprite <- readSprite mac (SI (nat8 i))
        drawSprite xy sprite palette
    |
      x <- [0..7]
    , y <- [0..7]
    ]

setSquare :: Int -> XY (E Nat) -> RGB (E Nat) -> Eff ()
setSquare width loc col = do
  let dels = [ XY{x,y} | x <- map nat8 [0..width-1], y <- map nat8 [0..width-1] ]
  sequence_ [do xy <- addXY loc offset; SetPixel xy col | offset <- dels]


-- draw a sprite selected and positioned by the spriteInfo/spriteXY rams
drawSpriteIndex :: Mac -> Int -> Eff ()
drawSpriteIndex mac@Mac{ramDump,xyDump} i = do
  info <- ReadRom ramDump (eSized 12 (0xff0 + 2 * fromIntegral i))
  palb <- ReadRom ramDump (eSized 12 (0xff1 + 2 * fromIntegral i))
  xLoc <- ReadRom xyDump (eSized 4 (0 + 2 * fromIntegral i))
  yLoc <- ReadRom xyDump (eSized 4 (1 + 2 * fromIntegral i))
  palette <- readPalette mac (PaletteIndex (mod64 palb))
  let _yFlip = info `index` 0
  let _xFlip = info `index` 1
  let spriteIndex = div4 info
  sprite <- readSprite mac (SI spriteIndex)
  let xMax = eSized 9 (28*8 + 15)
  let yMax = eSized 9 (32*8 + 16)
  x <- Minus xMax xLoc
  y <- Minus yMax yLoc
  let xy = XY { x, y }
  drawSprite xy sprite palette

div4 :: E Nat -> E Nat
div4 i = combine (reverse (drop 2 (reverse (split i))))


drawSprite :: XY (E Nat) -> Sprite -> Palette -> Eff ()
drawSprite xy sprite palette = do
  let (Sprite piis) = sprite --if xFlip||yFlip then sprite else sprite -- TODO flip!
  xys <- sequence [ addXY xy (XY { x = nat8 xd, y = nat8 yd })
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

drawTiles :: Mac -> Eff ()
drawTiles mac = do
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
    [ drawTile mac xy i | (xy,i::Int) <- top ++ mid ++ bot ]


-- draw a tile selected by the vram
drawTile :: Mac -> XY Int -> Int -> Eff ()
drawTile mac@Mac{ramDump} xy i = do
  byteT <- ReadRom ramDump (eSized 16 (fromIntegral i))
  tile <- readTile mac (TI byteT)
  byteP <- ReadRom ramDump (eSized 16 (0x400 + fromIntegral i))
  let six = mod64 byteP
  let pi = PaletteIndex six
  palette <- readPalette mac pi
  drawTile1 (fmap nat9 xy) tile palette

mod64 :: E Nat -> E Nat
mod64 i = combine (reverse (take 6 (reverse (split i))))

-- draw a tile selected by index
drawTile1 :: XY (E Nat) -> Tile -> Palette -> Eff ()
drawTile1 xy tile palette = do
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

newtype SpriteIndex = SI (E Nat) -- 0..63
data Sprite = Sprite [PaletteItemIndex] -- #256 (16x16) for each sprite pixel

readSprite :: Mac -> SpriteIndex -> Eff Sprite
readSprite Mac{spriteRom} (SI i) = do
  let
    readStrip :: Int -> Eff [PaletteItemIndex]
    readStrip off = do
      let i64 = times64 i -- 64 bytes per sprite
      a <- Plus i64 (eSized 6 off)
      byte <- ReadRom spriteRom a
      pure $ decodeTileByte byte
  layer1 <- mapM readStrip (reverse ([8..8+7] ++ [32+8+0..32+8+7]))
  layer2 <- mapM readStrip (reverse ([16..16+7] ++ [32+16+0..32+16+7]))
  layer3 <- mapM readStrip (reverse ([24..24+7] ++ [32+24+0..32+24+7]))
  layer4 <- mapM readStrip (reverse ([0..7] ++ [32+0..32+7]))
  let piis = concat [ concat (transpose layer) | layer <- [layer1,layer2,layer3,layer4] ]
  pure $ Sprite piis

times64 :: E Nat -> E Nat
times64 i = combine (split i ++ [b0,b0,b0,b0,b0,b0])

newtype TileIndex = TI (E Nat) -- 0..255
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
  r <- muxBits sel yr nr
  g <- muxBits sel yg ng
  b <- muxBits sel yb nb
  pure $ RGB { r, g, b }

readTile :: Mac -> TileIndex -> Eff Tile
readTile Mac{tileRom} (TI i) = do
  let
    readStrip off = do
      let i16 = times16 i -- 16 bytes per tile
      a <- Plus i16 (eSized 4 off)
      byte <- ReadRom tileRom a
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

readPalette :: Mac -> PaletteIndex -> Eff Palette
readPalette mac@Mac{palRom} (PaletteIndex i) = do
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
      byte <- ReadRom palRom a
      let ci = CI byte
      readColour mac ci

times4 :: E Nat -> E Nat
times4 i = do combine (split i ++ [b0,b0])

newtype ColourIndex = CI (E Nat)

readColour :: Mac -> ColourIndex -> Eff (RGB (E Nat))
readColour Mac{colRom} (CI i) = do
  byte <- ReadRom colRom i
  decodeAsRGB byte

decodeAsRGB :: E Nat -> Eff (RGB (E Nat))
decodeAsRGB w = do
  let
    bit :: Int -> Int -> Eff (E Nat)
    bit i v = do
      let c = w `index` i
      muxBits c (nat8 v) (nat8 0)
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

----------------------------------------------------------------------
-- muxing

-- TODO: type with explicit yes/no fields, so less likely to mess up mux wiring
-- class Muxable, or muxable for any functor?

muxBits :: E Bit -> E [Bit] -> E [Bit] -> Eff (E [Bit])
muxBits sel yes no = Mux sel YN {yes,no}
_muxBits :: E Bit -> E [Bit] -> E [Bit] -> Eff (E [Bit])
_muxBits sel yes no = do
  let ys = split yes
  let ns = split no
  combine <$> sequence [ mux sel y n | (y,n) <- zipChecked ys ns ]

zipChecked :: [a] -> [b] -> [(a,b)]
zipChecked xs ys = do
  let xn = length xs
  let yn = length ys
  if xn /= yn then error (show ("zipChecked",xn,yn)) else
    zip xs ys

mux :: E Bit -> E Bit -> E Bit -> Eff (E Bit)
mux sel yes no = do
  selb <- notG sel
  a <- andG sel yes
  b <- andG selb no
  orG a b

----------------------------------------------------------------------
-- gates

orG :: E Bit -> E Bit -> Eff (E Bit)
orG x y = do
  xb <- notG x
  yb <- notG y
  w <- andG xb yb
  notG w

notG :: E Bit -> Eff (E Bit)
notG = pure . eNot

andG :: E Bit -> E Bit -> Eff (E Bit)
andG = And

----------------------------------------------------------------------
-- numbers

nat8,nat9 :: Int -> E Nat
nat8 = eSized 8
nat9 = eSized 9

b0,b1 :: E Bit
b0 = eLit 1 B0
b1 = eLit 1 B1
