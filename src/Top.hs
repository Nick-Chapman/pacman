
module Top (main) where

import InstructionSet (theDecodeTable)
import PacEmu (DisControl(..))
import System.IO (stdout)
import qualified Graphics (main)
import qualified Mem (init)
import qualified PacEmu as Pac (init,State,emulateOneFrame,Conf(..))

main :: IO ()
main = do
  let _ = print theDecodeTable
  let _ = (stdout,DisOff)
  let _ = Graphics.main
  emulate
  pure ()

emulate :: IO ()
emulate = do
  mem <- Mem.init
  let s0 = Pac.init mem
  loop 0 s0
  where
    loop :: Int -> Pac.State -> IO ()
    loop frame ps = do
      putStrLn $ "frame: " ++ show frame
      if (frame == 248) then pure () else do -- when to stop
        let trace = if frame == 247 then Just (stdout, DisOff) else Nothing -- when to trace
        ps <- Pac.emulateOneFrame Pac.Conf { trace } ps
        loop (frame+1) ps
