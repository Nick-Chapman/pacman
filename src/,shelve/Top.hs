
module Top (main) where

import System.Environment (getArgs)
import InstructionSet (theDecodeTable)
import PacEmu (DisControl(..))
import System.IO (stdout)
import qualified Graphics (mock,emulate)
import qualified Mem (init)
import qualified PacEmu as Pac (init,State,emulateOneFrame,Conf(..))

import qualified Hdl_play as Hdl (main)
import qualified NewGraphics (mock)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    DecodeTable -> print theDecodeTable
    Trace -> traceEmu
    Mock -> Graphics.mock
    Graphics -> Graphics.emulate
    Hdl -> Hdl.main
    Video -> NewGraphics.mock

data Mode = DecodeTable | Trace | Mock | Graphics | Hdl | Video

parseArgs :: [String] -> Mode
parseArgs = \case
  ["tab"] -> DecodeTable
  ["trace"] -> Trace
  ["mock"] -> Mock
  ["graphics"] -> Graphics
  ["hdl"] -> Hdl
  [] -> Video
  xs -> error (show ("parseArgs",xs))

traceEmu :: IO ()
traceEmu = do
  mem <- Mem.init
  let s0 = Pac.init mem
  loop 0 s0
  where
    loop :: Int -> Pac.State -> IO ()
    loop frame ps = do
      putStrLn $ "frame: " ++ show frame
      if (frame == 248) then pure () else do -- when to stop
        let trace = if frame == 247 then Just (stdout, DisOn) else Nothing -- when to trace
        ps <- Pac.emulateOneFrame Pac.Conf { trace } ps
        loop (frame+1) ps
