module EmulateWithSdl (main) where

import Code (Code,State,Keys(..),Picture(..))
import Control.Concurrent (threadDelay)
import Control.DeepSeq (deepseq)
import Control.Monad (when)
import Data.List.Extra (groupSort)
import Data.Map (Map)
import Foreign.C.Types (CInt)
import GHC.Int (Int64)
import Prelude hiding (init)
import SDL (Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=))
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
import Text.Printf (printf)
import Value (ScreenSpec(..),Key(..),XY(..),RGB(..))
import qualified Code (initialize,runForOneFrame)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text (pack)
import qualified SDL

data World = World
  { state :: State
  , keys :: Keys
  , frame :: Int
  , accNanos :: Int64
  }

main :: Code -> Bool -> IO ()
main code accpix = do
  (ss,context,state,prog) <- Code.initialize code
  let! _ = keyMapTable
  SDL.initializeAll
  let fi = fromIntegral
  let ScreenSpec{sf,size=XY{x=screenW,y=screenH}} = ss
  let offset = 8

  let windowSize = V2 (fi sf * (2*offset + fi screenW)) (fi sf * (2*offset + fi screenH))
  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize }
  win <- SDL.createWindow (Text.pack "PacMan") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
  let assets = DrawAssets { renderer, ss, offset, accpix }
  let keys = Keys { pressed = Set.empty }
  let world0 = World { state, keys, frame = 0, accNanos = 0 }
  let
    loop :: World -> IO ()
    loop World{state,keys,frame,accNanos} = do

      (state,xNanos) <- measureNanos $ do
        x <- Code.runForOneFrame prog context state keys
        let (picture,state) = x `deepseq` x
        drawEverything assets picture
        pure state

      events <- SDL.pollEvents
      let interesting = [ i | e <- events, i <- interestingOf e ]
      if Quit `elem` interesting then pure () else do --quit
      keys <- pure $ foldl insertInteresting keys interesting
      let _ = threadDelay (1000000 `div` 60) -- 1/60 sec

      let world = World { state, keys, frame = frame+1, accNanos = accNanos + xNanos }
      printStatLine ss world
      loop world

  setColor renderer darkGrey
  SDL.clear renderer

  loop world0
  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit


printStatLine :: ScreenSpec -> World -> IO ()
printStatLine ScreenSpec{emuSecsPerFrame} World{frame,accNanos} = do
  let
    gig :: Double = 1_000_000_000
    -- a real frame should take 1/60s, but we are only doing 1/264 of that
    -- this info should come from the system under emu
    emulatedSecs :: Double = fromIntegral frame * emuSecsPerFrame
    elaspedSecs :: Double = fromIntegral accNanos / gig
    --speedup = emulatedSecs / elaspedSecs
    slowdown = elaspedSecs / emulatedSecs
    line =
      printf "%d, emu %.03f, elap %.03f, slowX %.f"
      frame emulatedSecs elaspedSecs slowdown
  putStrLn line

measureNanos :: IO a -> IO (a, Int64)
measureNanos io = do
  before <- getTime Monotonic
  a <- io
  after <- getTime Monotonic
  let TimeSpec{sec,nsec} = after - before
  let nanos = gig * sec + nsec
  return (a,nanos)
  where gig = 1_000_000_000


data InterestingEvent = Press Key | Release Key | Quit deriving Eq

insertInteresting :: Keys -> InterestingEvent -> Keys
insertInteresting ks@Keys{pressed} = \case
  Press key -> ks { pressed = Set.insert key pressed }
  Release key -> ks { pressed = Set.delete key pressed }
  Quit -> ks

interestingOf :: SDL.Event -> [InterestingEvent]
interestingOf = \case
  SDL.Event _t SDL.QuitEvent -> [Quit]
  SDL.Event _ (SDL.KeyboardEvent ke) -> do
    let code = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
    let motion = SDL.keyboardEventKeyMotion ke
    case (code,motion) of
      (SDL.KeycodeEscape,SDL.Released) -> [Quit]
      _ ->
        case Map.lookup code keyMapTable of
          Nothing -> []
          Just key -> do
            case motion of
              SDL.Pressed -> [Press key]
              SDL.Released -> [Release key]
  SDL.Event _ _ ->
    []


keyMapTable :: Map SDL.Keycode Key
keyMapTable = Map.fromList ys
  where
    xs = [ (by, key) | key <- [minBound..maxBound], by <- keyedBy key ]
    ys = [ (code, expectUnique code keys) | (code,keys) <- groupSort xs ]
    expectUnique code = \case
      [key] -> key
      keys -> error $
        unlines $
        ("bad keyMapTable: " <> show code) : [ "--> " <> show key | key <- keys ]

    -- | define the reverse mapping to be sure we are complete
    keyedBy :: Key -> [SDL.Keycode]
    keyedBy = \case
      KeyZ -> [SDL.KeycodeZ]
      KeyX -> [SDL.KeycodeX]
      KeyEnter -> [SDL.KeycodeReturn]
      KeyShift -> [SDL.KeycodeLShift, SDL.KeycodeRShift]


data DrawAssets = DrawAssets
  { renderer :: Renderer
  , ss :: ScreenSpec
  , offset :: CInt
  , accpix :: Bool
  }

drawEverything :: DrawAssets -> Picture -> IO ()
drawEverything assets@DrawAssets{renderer=r,accpix} picture = do
  when (not accpix) $ do
    setColor r darkGrey
    SDL.clear r
  renderPicture assets picture
  SDL.present r

renderPicture :: DrawAssets -> Picture  -> IO ()
renderPicture DrawAssets{renderer=r,ss,offset} = traverse
  where
    traverse :: Picture -> IO ()
    traverse = \case
      Pictures pics -> mapM_ traverse pics
      Draw (XY{x,y}) rgb -> do
        let fi = fromIntegral
        let ScreenSpec{sf,size=XY{x=maxX,y=maxY}} = ss
        --putStrLn (show ("pixel",(maxX,maxY),(x,y),rgb))
        when (x<0) $ error (show ("pixel:x<0",x))
        when (y<0) $ error (show ("pixel:y<0",y))
        when (x>=maxX) $ error (show ("pixel:x>=maxX",x,maxX))
        when (y>=maxY) $ error (show ("pixel:y>=maxY",y,maxY))
        setColor r rgb
        let x' = fi sf * (fromIntegral x + offset)
        let y' = fi sf * (fromIntegral y + offset)
        let rect = SDL.Rectangle (SDL.P (V2 x' y')) (V2 (fi sf) (fi sf))
        SDL.fillRect r (Just rect)

darkGrey :: RGB Int
darkGrey = RGB { r = 50, g = 50, b = 50 }

setColor :: SDL.Renderer -> RGB Int -> IO ()
setColor r c = SDL.rendererDrawColor r $= fromRGB c
  where
    fi = fromIntegral
    fromRGB RGB {r,g,b} = V4 (fi r) (fi g) (fi b) 255
