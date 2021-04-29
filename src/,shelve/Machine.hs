
-- This was in Graphics.hs -- what is the best name/place for this?
module Machine (Machine(..),initMachine) where

import Mem (Mem)
import Rom (Rom)
import qualified PacEmu as Pac
import qualified Rom (load)

data Machine = Machine
  { colRom :: Rom
  , palRom :: Rom
  , tileRom :: Rom
  , spriteRom :: Rom
  , mem :: Mem
  , ps :: Pac.State
  }

initMachine :: Mem -> IO Machine
initMachine mem = do
  colRom <- Rom.load 32 "roms/82s123.7f"
  palRom <- Rom.load 256 "roms/82s126.4a"
  tileRom <- Rom.load 4096 "roms/pacman.5e"
  spriteRom <- Rom.load 4096 "roms/pacman.5f"
  let ps = Pac.init mem
  pure $ Machine { colRom, palRom, tileRom, spriteRom, mem, ps }
