
module Top (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (ap,liftM)
import Data.Bits (testBit)
import Data.List (transpose)
import Data.Map (Map)
import Foreign.C.Types (CInt)
import Rom (Rom)
import SDL (Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=))
import Addr (Addr(..))
import Byte (Byte(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text (pack)
import qualified Rom (load,lookup)
import qualified SDL

main :: IO ()
main = do
  putStrLn "*pacman*"
  m <- initMachine
  picture <- run m seeScreen
  display picture

seeScreen :: Prog Picture
seeScreen = do
  drawTiles
  sequence_ [drawSprite i | i <- [0..7]]
  screen <- GetScreen
  pure $ pictureScreen screen

drawSprite :: Int -> Prog ()
drawSprite i = do
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
  let (Sprite piis) = if xFlip||yFlip then undefined else sprite -- TODO
  let xys = [ XY (x+x0) (y+y0) | y <- [0..15] , x <- [0..15]]
  sequence_ [ SetPixel xy rgb
            | (xy,pii) <- zip xys piis
            , let rgb = resolvePaletteItemIndex palette pii
            ]

drawTiles :: Prog ()
drawTiles = do
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

baseVideoRam :: Addr
baseVideoRam = 0x4000

drawTile :: XY -> Int -> Prog ()
drawTile xy i = do
  byteT <- ReadMem (baseVideoRam + fromIntegral i)
  tile <- readTile (TI (fromIntegral byteT))
  byteP <- ReadMem (baseVideoRam + 0x400 + fromIntegral i)
  palette <- readPalette (makePI (fromIntegral byteP))
  let XY{x=x0,y=y0} = xy
  let (Tile piis) = tile
  let xys = [ XY (x+x0) (y+y0) | y <- [0..7] , x <- [0..7]]
  sequence_ [ SetPixel xy rgb
            | (xy,pii) <- zip xys piis
            , let rgb = resolvePaletteItemIndex palette pii
            ]


newtype SpriteIndex = SI Int deriving (Num,Enum,Integral,Real,Ord,Eq) -- 0..63

data Sprite = Sprite [PaletteItemIndex] -- #(16x16)

readSprite :: SpriteIndex -> Prog Sprite
readSprite (SI i) = do
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
  pure $ Sprite piis


newtype TileIndex = TI Int deriving (Num,Enum,Integral,Real,Ord,Eq) -- 0..255

data Tile = Tile [PaletteItemIndex] -- #(8x8)

data PaletteItemIndex = P0 | P1 | P2 | P3

resolvePaletteItemIndex :: Palette -> PaletteItemIndex -> RGB
resolvePaletteItemIndex Palette{p0,p1,p2,p3} = \case
  P0 -> p0; P1 -> p1; P2 -> p2; P3 -> p3

makePII :: (Bool,Bool) -> PaletteItemIndex
makePII = \case
  (False,False) -> P0
  (False,True) -> P1
  (True,False) -> P2
  (True,True) -> P3

readTile :: TileIndex -> Prog Tile
readTile (TI i) = do
  let bytesPerTile = 16
  let
    readStrip off = do
      byte <- ReadTileRom (bytesPerTile*i + off)
      pure $ decodeTileByte byte
  bot <- mapM readStrip (reverse [0..7])
  top <- mapM readStrip (reverse [8..15])
  let piis = concat (transpose top) ++ concat (transpose bot)
  pure $ Tile piis

decodeTileByte :: Byte -> [PaletteItemIndex] -- #4
decodeTileByte (Byte w) = do
  let pick i = w `testBit` i
  [ makePII (pick b1, pick b0) | (b1,b0) <- zip [7,6,5,4] [3,2,1,0] ]

newtype PaletteIndex = PI Int deriving (Num,Enum,Integral,Real,Ord,Eq)

makePI :: Int -> PaletteIndex
makePI i = PI (i `mod` maxPI) where maxPI = 64

data Palette = Palette { p0 :: RGB, p1 :: RGB, p2 :: RGB, p3 :: RGB }

readPalette :: PaletteIndex -> Prog Palette
readPalette (PI i) = do
  p0 <- readItem 0
  p1 <- readItem 1
  p2 <- readItem 2
  p3 <- readItem 3
  pure $ Palette { p0, p1, p2, p3 }
  where
    readItem off = do
      byte <- ReadPalRom (4*i + off)
      let ci = fromIntegral byte
      readColour ci

newtype ColourIndex = CI Int deriving (Num,Integral,Real,Enum,Ord,Eq)

readColour :: ColourIndex -> Prog RGB
readColour (CI i) = do
  byte <- ReadColRom i
  pure $ decodeAsRGB byte

decodeAsRGB :: Byte -> RGB
decodeAsRGB (Byte w) = do
  let
    bit i v = if w `testBit` i then v else 0
    r = bit 0 0x21 + bit 1 0x47 + bit 2 0x97
    g = bit 3 0x21 + bit 4 0x47 + bit 5 0x97
    b = bit 6 0x51 + bit 7 0xAE
  RGB { r, g, b }

data Prog a where
  Ret :: a -> Prog a
  Bind :: Prog a -> (a -> Prog b) -> Prog b
  Trace :: String -> Prog ()
  ReadColRom :: Int -> Prog Byte
  ReadPalRom :: Int -> Prog Byte
  ReadTileRom :: Int -> Prog Byte
  ReadSpriteRom :: Int -> Prog Byte
  ReadMem :: Addr -> Prog Byte
  SetPixel :: XY -> RGB -> Prog ()
  GetScreen :: Prog Screen

instance Functor Prog where fmap = liftM
instance Applicative Prog where pure = return; (<*>) = ap
instance Monad Prog where return = Ret; (>>=) = Bind

data Machine = Machine
  { colRom :: Rom
  , palRom :: Rom
  , tileRom :: Rom
  , spriteRom :: Rom
  , mem :: Mem
  }

data Mem = Mem { dump :: Rom }

readMem :: Mem -> Addr -> Byte
readMem Mem{dump} a =
  case Map.lookup a spriteAddrDump of
    Just b -> b
    Nothing -> Rom.lookup dump (fromIntegral (a - baseVideoRam))


spriteAddrDump :: Map Addr Byte
spriteAddrDump = Map.fromList
  [ (0x4ff0, 0x00)
  , (0x4ff1, 0x00)
  , (0x4ff2, 0x94)
  , (0x4ff3, 0x01)
  , (0x4ff4, 0x94)
  , (0x4ff5, 0x03)
  , (0x4ff6, 0x8c)
  , (0x4ff7, 0x05)
  , (0x4ff8, 0x8c)
  , (0x4ff9, 0x07)
  , (0x4ffa, 0xb8)
  , (0x4ffb, 0x09)
  , (0x4ffc, 0xfc)
  , (0x4ffd, 0x00)
  , (0x4ffe, 0x00)
  , (0x4fff, 0x00)
  , (0x5060, 0x00)
  , (0x5061, 0x00)
  , (0x5062, 0xb6)
  , (0x5063, 0x8c)
  , (0x5064, 0xa1)
  , (0x5065, 0xa4)
  , (0x5066, 0x97)
  , (0x5067, 0x8e)
  , (0x5068, 0x77)
  , (0x5069, 0x8e)
  , (0x506a, 0xa3)
  , (0x506b, 0x2c)
  , (0x506c, 0x07)
  , (0x506d, 0x08)
  , (0x506e, 0x00)
  , (0x506f, 0x00)
  ]

initMachine :: IO Machine
initMachine = do
  colRom <- Rom.load 32 "roms/82s123.7f"
  palRom <- Rom.load 256 "roms/82s126.4a"
  tileRom <- Rom.load 4096 "roms/pacman.5e"
  spriteRom <- Rom.load 4096 "roms/pacman.5f"
  dump <- Rom.load 2048 "dump"
  let mem = Mem { dump }
  pure $ Machine { colRom, palRom, tileRom, spriteRom, mem } where

data Screen = Screen { m :: Map XY RGB }

screen0 :: Screen
screen0 = Screen { m = Map.empty }

setScreenPixel :: Screen -> XY -> RGB -> Screen
setScreenPixel Screen{m} xy rgb = Screen { m = Map.insert xy rgb m }

pictureScreen :: Screen -> Picture
pictureScreen Screen{m} = Pictures [ Draw (shift xy) rgb | (xy,rgb) <- Map.toList m ]
  where
    shift XY{x,y} = XY { x = x+8, y = y+8 }

data State = State
  { screen :: Screen
  }

state0 :: State
state0 = State { screen = screen0 }

run :: Machine -> Prog a -> IO a
run Machine{colRom,palRom,tileRom,spriteRom,mem} prog0 = eval prog0 state0 (\_ -> pure)
  where
  eval :: Prog b -> State -> (State -> b -> IO a) -> IO a
  eval prog s k = case prog of
    Ret x -> k s x
    Bind p f -> eval p s $ \s a -> eval (f a) s k
    Trace mes -> do print mes; k s ()
    ReadColRom i -> k s (Rom.lookup colRom i)
    ReadPalRom i -> k s (Rom.lookup palRom i)
    ReadTileRom i -> k s (Rom.lookup tileRom i)
    ReadSpriteRom i -> k s (Rom.lookup spriteRom i)
    ReadMem a -> k s (readMem mem a)
    SetPixel xy rgb -> k s { screen = setScreenPixel (screen s) xy rgb } ()
    GetScreen -> k s (screen s)

data Picture where
  Draw :: XY -> RGB -> Picture
  Pictures :: [Picture] -> Picture

data XY = XY { x :: Int, y :: Int } deriving (Eq,Ord,Show)
data RGB = RGB { r :: Byte, g :: Byte, b :: Byte } deriving Show

display :: Picture -> IO ()
display picture = do
  SDL.initializeAll
  let sf = 3
  let screenW = 8 * (28 + 2)
  let screenH = 8 * (36 + 2)
  let windowSize = V2 (sf * screenW) (sf * screenH)
  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize }
  win <- SDL.createWindow (Text.pack "PacMan") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
  let assets = DrawAssets { renderer, sf }
  let
    loop :: Int -> IO ()
    loop i = do
      events <- SDL.pollEvents
      if containsQuit events then pure () else do
        drawEverything assets picture
        threadDelay (1000000 `div` 60)
        loop (i+1)
  loop 0
  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit

containsQuit :: [SDL.Event] -> Bool
containsQuit = \case
  [] -> False
  e1:es -> do
    case e1 of
      SDL.Event _t SDL.QuitEvent -> True
      SDL.Event _ (SDL.KeyboardEvent ke) -> do
        let code = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
        let motion = SDL.keyboardEventKeyMotion ke
        case (code,motion) of
          (SDL.KeycodeEscape,SDL.Released) -> True
          _ -> containsQuit es
      SDL.Event _ _ ->
        containsQuit es

data DrawAssets = DrawAssets
  { renderer :: Renderer
  , sf :: CInt
  }

drawEverything :: DrawAssets -> Picture -> IO ()
drawEverything assets@DrawAssets{renderer=r} picture = do
  setColor r darkGrey
  SDL.clear r
  renderPicture assets picture
  SDL.present r

renderPicture :: DrawAssets -> Picture  -> IO ()
renderPicture DrawAssets{renderer=r,sf} = traverse
  where
    traverse :: Picture -> IO ()
    traverse = \case
      Pictures pics -> mapM_ traverse pics
      Draw (XY{x,y}) rgb -> do
        setColor r rgb
        let x' = sf * fromIntegral x
        let y' = sf * fromIntegral y
        let rect = SDL.Rectangle (SDL.P (V2 x' y')) (V2 sf sf)
        SDL.fillRect r (Just rect)

darkGrey :: RGB
darkGrey = RGB { r = 20, g = 20, b = 20 }

setColor :: SDL.Renderer -> RGB -> IO ()
setColor r c = SDL.rendererDrawColor r $= fromRGB c
  where
    fromRGB RGB {r,g,b} = V4 (unByte r) (unByte g) (unByte b) 255
