
module Top (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (ap,liftM)
import Data.Bits (testBit)
import Data.List (transpose)
import Foreign.C.Types (CInt)
import Rom (Rom)
import SDL (Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=))
import Types (Byte(..))
import qualified Data.Text as Text (pack)
import qualified Rom (load,lookup)
import qualified SDL

main :: IO ()
main = do
  putStrLn "*pacman*"
  m <- initMachine
  picture <- run m seeRoms
  display picture


seeRoms :: Prog Picture
seeRoms = do
  cols <- mapM readColour [0..15]
  pals <- mapM readPalette [0..31]
  tiles <- mapM readTile [0..255]
  somePalette <- readPalette 1
  pure $ Pictures (
    [ seeColour i col | (i,col) <- zip [0..] cols ] ++
    [ seePalette i pal | (i,pal) <- zip [0..] pals ] ++
    [ seeTile somePalette i tile | (i,tile) <- zip [0..] tiles ]
    )

seeColour :: Int -> RGB -> Picture
seeColour i rgb = do
  let size = 20
  let xy = XY {x = 10 + size * i, y = 10}
  square (size-3) xy rgb

seePalette :: Int -> Palette -> Picture
seePalette i Palette{p0,p1,p2,p3} = do
  let size = 10
  Pictures
    [ square (size-2) xy rgb
    | (rgb,yoff) <- zip [p0,p1,p2,p3] [30,20,10,0]
    , let xy = XY {x = 10 + size * i, y = 50 + yoff}
    ]

seeTile :: Palette -> Int -> Tile -> Picture
seeTile palette i (Tile piis) = do
  -- arrange 256 ties in a 16x16 grid for display
  let (x0,y0) = (100 + 10 * (i `mod` 16), 100 + 10 * (i `div` 16))
  let size = 8
  let xys = [ XY (x+x0) (y+y0) | y <- [0..size-1] , x <- [0..size-1]]
  Pictures [ Draw xy rgb
           | (xy,pii) <- zip xys piis
           , let rgb = resolvePaletteItemIndex palette pii
           ]

square :: Int -> XY -> RGB -> Picture
square size XY{x=x0,y=y0} rgb =
  Pictures [ Draw xy rgb
           | x <- [0..size-1]
           , y <- [0..size-1]
           , let xy = XY (x+x0) (y+y0)
           ]


newtype TileIndex = TI Int deriving (Num,Enum,Integral,Real,Ord,Eq)

data Tile = Tile [PaletteItemIndex] -- #64

data PaletteItemIndex = P0 | P1 | P2 | P3

resolvePaletteItemIndex :: Palette -> PaletteItemIndex -> RGB
resolvePaletteItemIndex Palette{p0,p1,p2,p3} = \case
  P0 -> p0
  P1 -> p1
  P2 -> p2
  P3 -> p3

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

instance Functor Prog where fmap = liftM
instance Applicative Prog where pure = return; (<*>) = ap
instance Monad Prog where return = Ret; (>>=) = Bind

data Machine = Machine
  { colRom :: Rom
  , palRom :: Rom
  , tileRom :: Rom
  }

initMachine :: IO Machine
initMachine = do
  colRom <- Rom.load 32 "roms/82s123.7f"
  palRom <- Rom.load 256 "roms/82s126.4a"
  tileRom <- Rom.load 4096 "roms/pacman.5e"
  pure $ Machine { colRom, palRom, tileRom } where

run :: Machine -> Prog a -> IO a
run Machine{colRom,palRom,tileRom} p = eval p where
  eval :: Prog a -> IO a
  eval = \case
    Ret x -> pure x
    Bind p f -> do a <- eval p; eval (f a)
    Trace s -> print s
    ReadColRom i -> pure $ Rom.lookup colRom i
    ReadPalRom i -> pure $ Rom.lookup palRom i
    ReadTileRom i -> pure $ Rom.lookup tileRom i

data Picture where
  Draw :: XY -> RGB -> Picture
  Pictures :: [Picture] -> Picture

data XY = XY { x :: Int, y :: Int } deriving Show
data RGB = RGB { r :: Byte, g :: Byte, b :: Byte } deriving Show

display :: Picture -> IO ()
display picture = do
  SDL.initializeAll
  let sf = 3
  let screenW = 340
  let screenH = 300
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
