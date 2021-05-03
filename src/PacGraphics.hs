
module PacGraphics (debug,screen) where

import Data.List (transpose)
import System
import Value

debug :: System
debug =
  DeclareRom (RomSpec { path = "roms/82s123.7f", size = 32 }) $ \colRom -> do
  DeclareRom (RomSpec { path = "roms/82s126.4a", size = 256 }) $ \palRom -> do
  DeclareRom (RomSpec { path = "roms/pacman.5e", size = 4096 }) $ \tileRom -> do
  --DeclareRom (RomSpec { path = "roms/pacman.5f", size = 4096 }) $ \sprite -> do
  let mac = Mac { colRom, palRom, tileRom }
  FrameEffect $ do
    seeCols mac
    seePals mac
    seeTiles mac

screen :: System
screen =
  DeclareRom (RomSpec { path = "roms/82s123.7f", size = 32 }) $ \colRom -> do
  DeclareRom (RomSpec { path = "roms/82s126.4a", size = 256 }) $ \palRom -> do
  DeclareRom (RomSpec { path = "roms/pacman.5e", size = 4096 }) $ \tileRom -> do
  --DeclareRom (RomSpec { path = "roms/pacman.5f", size = 4096 }) $ \sprite -> do
  let mac = Mac { colRom, palRom, tileRom }
  FrameEffect $ do
    seeScreen mac -- TODO: goal

data Mac = Mac
  { colRom :: RomId
  , palRom :: RomId
  , tileRom :: RomId
  }

----------------------------------------------------------------------
-- debugging: see cols,palettes,tiles

seeCols :: Mac -> Eff ()
seeCols Mac{colRom} = sequence_
  [ do
      b <- ReadRomByte colRom (eSized (Size 4) i)
      col <- decodeAsRGB b
      let xy = XY { x = byte (14 * i), y = byte 0 }
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
      let xy = XY { x = byte (w * i), y = byte (yoff + w * j) }
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
        let yoff = 80
        let i = 16*y + x
        let xy = XY { x = byte (30 + 10*x), y = byte (yoff + 10*y) }
        tile <- readTile mac (TI (byte i))
        drawTile1 xy tile palette
    |
      x <- [0..15]
    , y <- [0..15]
    ]

setSquare :: Int -> XY (E Nat) -> RGB (E Nat) -> Eff ()
setSquare width loc col = do
  let dels = [ XY{x,y} | x <- map byte [0..width-1], y <- map byte [0..width-1] ]
  sequence_ [do xy <- addXY loc offset; SetPixel xy col | offset <- dels]


----------------------------------------------------------------------
-- TODO: The screen with correctly layed out tiles + sprites

seeScreen :: Mac -> Eff ()
seeScreen _mac = do
  --drawTiles mac
  --let _ = sequence_ [drawSprite i | i <- [0..7]]
  let _ = undefined drawSprite
  pure ()

drawSprite :: Int -> Eff ()
drawSprite i =
  undefined i readPalette readSprite resolvePaletteItemIndex

{-drawSprite i = do
  xLoc <- ReadMem (0x5060 + 2 * fromIntegral i)
  yLoc <- ReadMem (0x5061 + 2 * fromIntegral i)
  info <- ReadMem (0x4ff0 + 2 * fromIntegral i)
  palb <- ReadMem (0x4ff1 + 2 * fromIntegral i)
  let yFlip = unByte info `testBit` 0
  let xFlip = unByte info `testBit` 1
  let spriteIndex = fromIntegral (info `div` 4)
  let x0 = fromIntegral (28*8 + 15 - xLoc)
  let y0 = fromIntegral (32*8 + 16 - yLoc)
  palette <- readPalette (makePI (fromIntegral palb))
  sprite <- readSprite (SI spriteIndex)
  let (Sprite piis) = if xFlip||yFlip then sprite else sprite -- TODO flip!
  let xys = [ XY (x+x0) (y+y0) | y <- [0..15] , x <- [0..15]]
  sequence_ [ SetPixel xy rgb
            | (xy,pii) <- zip xys piis
            , let rgb = resolvePaletteItemIndex palette pii
            ]
-}

{-drawTiles :: Mac -> Eff ()
drawTiles mac = do
  undefined (drawTile mac)-}

{-do
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
    [ drawTile xy i | (xy,i) <- top ++ mid ++ bot ]
-}


{-
baseVideoRam :: Int
baseVideoRam = 0x4000

drawTile :: Mac -> XY (E Nat) -> Int -> Eff ()
drawTile mac xy i = do
  byteT <- ReadMem (ePos (baseVideoRam + fromIntegral i))
  tile <- readTile mac (TI byteT)
  byteP <- ReadMem (ePos (baseVideoRam + 0x400 + fromIntegral i))
  pi <- makePaletteIndex byteP
  palette <- readPalette mac pi
  drawTile1 xy tile palette
-}


drawTile1 :: XY (E Nat) -> Tile -> Palette -> Eff ()
drawTile1 xy tile palette = do
  let (Tile piis) = tile
  xys <- sequence [ do
                      let del = XY { x = byte xd, y = byte yd }
                      addXY xy del
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

newtype SpriteIndex = SI Int deriving (Num,Enum,Integral,Real,Ord,Eq) -- 0..63

data Sprite = Sprite [PaletteItemIndex] -- #(16x16)

readSprite :: SpriteIndex -> Eff Sprite
readSprite (SI i) = do
  undefined i Sprite
{-readSprite (SI i) = do
  let bytesPerSprite = 64
  let
    readStrip off = do
      byte <- ReadSpriteRom (bytesPerSprite*i + off)
      pure $ decodeTileByte byte
  layer1 <- mapM readStrip (reverse ([8..8+7] ++ [32+8+0..32+8+7]))
  layer2 <- mapM readStrip (reverse ([16..16+7] ++ [32+16+0..32+16+7]))
  layer3 <- mapM readStrip (reverse ([24..24+7] ++ [32+24+0..32+24+7]))
  layer4 <- mapM readStrip (reverse ([0..7] ++ [32+0..32+7]))
  let piis = concat [ concat (transpose layer) | layer <- [layer1,layer2,layer3,layer4] ]
  pure $ Sprite piis-}


newtype TileIndex = TI (E Nat) -- 0..255

data Tile = Tile [PaletteItemIndex] -- #255 (8x8) for each pixel in the tile

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
      i16 <- times16 i -- 16 bytes per tile
      a <- Plus i16 (eSized 4 off)
      byte <- ReadRomByte tileRom a
      decodeTileByte byte
  bot <- mapM readStrip (reverse [0..7])
  top <- mapM readStrip (reverse [8..15])
  let piis = concat (transpose top) ++ concat (transpose bot)
  pure $ Tile piis

times16 :: E Nat -> Eff (E Nat)
times16 i = do
  xs <- Split i
  Combine (xs ++ [b0,b0,b0,b0]) where b0 = E_Lit 1 B0

decodeTileByte :: E Nat -> Eff [PaletteItemIndex] -- #4
decodeTileByte w = do
  Split w >>= \case
    [a,b,c,d,e,f,g,h] -> pure [ PII a e , PII b f , PII c g , PII d h ]
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
      i4 <- times4 i
      a <- Plus i4 (eSized 2 off)
      byte <- ReadRomByte palRom a
      let ci = CI byte
      readColour mac ci

times4 :: E Nat -> Eff (E Nat)
times4 i = do
  xs <- Split i
  Combine (xs ++ [b0,b0]) where b0 = E_Lit 1 B0

newtype ColourIndex = CI (E Nat)

readColour :: Mac -> ColourIndex -> Eff (RGB (E Nat))
readColour Mac{colRom} (CI i) = do
  byte <- ReadRomByte colRom i
  decodeAsRGB byte

decodeAsRGB :: E Nat -> Eff (RGB (E Nat))
decodeAsRGB w = do
  let
    bit :: Int -> Int -> Eff (E Nat)
    bit i v = do
      c <- w `index` i
      muxBits c (byte v) (byte 0)
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
    let z = byte 0
    add3 x y z
  pure RGB { r, g, b }
  where add3 a b c = do ab <- Plus a b; Plus ab c

----------------------------------------------------------------------
-- muxing

-- TODO: type with explicit yes/no fields, so less likely to mess up mux wiring
-- class Muxable, or muxable for any functor?

muxBits :: E Bit -> E [Bit] -> E [Bit] -> Eff (E [Bit])
muxBits = Mux
_muxBits :: E Bit -> E [Bit] -> E [Bit] -> Eff (E [Bit])
_muxBits sel yes no = do
  ys <- Split yes
  ns <- Split no
  sequence [ mux sel y n | (y,n) <- zipChecked ys ns ] >>= Combine

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

byte :: Int -> E Nat
byte = eSized 8

eSized :: Size -> Int -> E Nat
eSized size i = E_Nat (sizedNat size i)

b0,b1 :: E Bit
b0 = E_Lit 1 B0
b1 = E_Lit 1 B1

