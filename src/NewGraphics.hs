
module NewGraphics (mock) where
{-
- re-code my screen-drawing-code using a Video.Eff
- convert that to a Video.Prog
- inspect the prog -- eventually it will become a C prog
- then simulate the prog
- This is the entry point for testing
        that the mock picture still works.
        and the startup graphics.
- Sep files for
    Video.hs -- generic code for Eff, Prog(residual), Picture
       compile :: Eff -> Prog (alt compile, with/out loop/rom unrolling/inlining)
       run :: Prog -> Picture
    EmuSDL.hs runs SDL emulator,given a Video.Prog. (cut from Graphics.hs)
    MyPacVideoSim.hs -- recode of my screen-decode-alg, conv .hs -> Video.Eff
   (Goal: One day, we will have a version derived from the HDL desc)
-}

import qualified MyPacVideoSim
import qualified EmuSdl
import qualified Video

import Machine
import Mem

mock :: IO ()
mock = do
  putStrLn "*pacman-mock(new graphics)*"
  let eff = MyPacVideoSim.seeCols
  let vprog = Video.compile eff
  --print vprog
  mem <- Mem.init
  m <- initMachine mem
  let runEmu = False
  EmuSdl.runDisplay runEmu vprog m
