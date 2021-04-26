
module EmuSdl (runDisplay) where

import Control.Concurrent (threadDelay)
import Data.Word8 (Word8)
import Foreign.C.Types (CInt)
import Machine (Machine(ps,mem))
import SDL (Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=))
import Video (Picture(..),XY(..),RGB(..))
import qualified Data.Text as Text (pack)
import qualified PacEmu as Pac
import qualified SDL
import qualified Video (Prog,run)

runDisplay :: Bool -> Video.Prog -> Machine -> IO ()
runDisplay doEmu vprog m0 = do
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
    loop :: Machine -> Int -> IO ()
    loop m frame = do
      events <- SDL.pollEvents
      if containsQuit events then pure () else do
        --putStrLn $ "frame: " ++ show frame
        let picture = Video.run vprog
        drawEverything assets picture
        let _ = threadDelay (1000000 `div` 60) -- no delay

        m <- case doEmu of
          False -> pure m
          True -> do
            ps <- Pac.emulateOneFrame Pac.Conf { trace = Nothing } (ps m)
            pure $ m { ps, mem = Pac.mem ps }

        loop m (frame+1)

  loop m0 0
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

darkGrey :: RGB Word8
darkGrey = RGB { r = 20, g = 20, b = 20 }

setColor :: SDL.Renderer -> RGB Word8 -> IO ()
setColor r c = SDL.rendererDrawColor r $= fromRGB c
  where
    fromRGB RGB {r,g,b} = V4 r g b 255
