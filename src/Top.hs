
module Top (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (ap,liftM)
import Data.Bits (testBit)
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
  picture <- run m seeColourSquares
  display picture

seeColourSquares :: Prog Picture
seeColourSquares =
  Pictures <$> sequence [ seeColourIndex ci | ci <- [0..15] ]

seeColourIndex :: ColourIndex -> Prog Picture
seeColourIndex ci = do
  byte <- ReadCol ci
  let rgb = decodeAsRGB byte
  let xy = XY {x = 10 + 20 * fromIntegral ci, y = 10}
  pure $ colSquare xy rgb

colSquare :: XY -> RGB -> Picture
colSquare XY{x=x0,y=y0} rgb =
  Pictures [ Draw xy rgb
           | x <- [0..17]
           , y <- [0..19]
           , let xy = XY (x+x0) (y+y0)
           ]

newtype ColourIndex = CI Int deriving (Num,Integral,Real,Enum,Ord,Eq)

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
  ReadCol :: ColourIndex -> Prog Byte

instance Functor Prog where fmap = liftM
instance Applicative Prog where pure = return; (<*>) = ap
instance Monad Prog where return = Ret; (>>=) = Bind

data Machine = Machine
  { colRom :: Rom
  }

initMachine :: IO Machine
initMachine = do
  colRom <- Rom.load 32 "roms/82s123.7f"
  pure $ Machine { colRom } where

run :: Machine -> Prog a -> IO a
run Machine{colRom} p = eval p where
  eval :: Prog a -> IO a
  eval = \case
    Ret x -> pure x
    Bind p f -> do a <- eval p; eval (f a)
    Trace s -> print s
    ReadCol (CI i) -> pure $ Rom.lookup colRom i

data Picture where
  Draw :: XY -> RGB -> Picture
  Pictures :: [Picture] -> Picture

data XY = XY { x :: Int, y :: Int } deriving Show
data RGB = RGB { r :: Byte, g :: Byte, b :: Byte } deriving Show

display :: Picture -> IO ()
display picture = do
  SDL.initializeAll
  let sf = 2
  let screenW = 340
  let screenH = 40
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
