{-# LANGUAGE RecordWildCards #-}

-- | System to decode a pacman screen. Based on a VHDL simulation model of the video decode hardware.

module ScreenDecodeHardware (vhdlSimulationModel) where

import DisplayTilesAndSprites (decodeAsRGB)
import MiscVHDL (b0,b1,slice,(&),if_,not)
import Pacman_RomsAndRams (withVideoRoms,RamDump(..),withRamDump)
import Pacman_video (pacman_video)
import Pacman_videoTiming (video_timing)
import Pacman_vram_addr (pacman_vram_addr)
import Prelude hiding (not,and,or,(<=))
import System (System(..),Eff(..),E,eSized)
import Value (ScreenSpec(..),defaultScreenSpec,XY(..),Bit(..))
import qualified Pacman_video as V (Inputs(..),Outputs(..),withRegs)
import qualified Pacman_videoTiming as VT (Regs(..),withRegs)

vhdlSimulationModel :: String -> System
vhdlSimulationModel suf = do
  withVideoRoms $ \roms -> do
  withRamDump suf $ \rams@RamDump{ram} -> do
  V.withRegs $ \regs -> do
  VT.withRegs $ \vtRegs -> do

  -- An internal ram of 256 nibbles used by the video decode hardware
  DeclareRam 256 $ \sprite_ram -> do

  let x = 224 -- (28*8)
  let y = 288 -- (36*8)

  let hTicks = 384 --y
  let vTicks = 264 --x
  let nTicks = hTicks * vTicks

  let ss = defaultScreenSpec { sf = 3, size = XY { x, y }, emuSecsPerFrame = 1.0 / 60 }

  FrameEffect ss $ do
    Repeat nTicks $ do

      let ena_6 :: E Bit = b1

      let VT.Regs{..} = vtRegs
      i_hcnt <- GetReg hcnt -- TODO: idea: intro type for reg/last-value
      i_vcnt <- GetReg vcnt
      i_hblank <- GetReg hblank
      i_vblank <- GetReg vblank

      video_timing vtRegs ena_6

      vram_addr_ab <- do
        pacman_vram_addr i_hcnt (i_vcnt `slice` (7,0))

      -- ignore the time when the CPU controls the bus.
      let i_ab = vram_addr_ab

      -- u_rams
      rams_data_out <-
        --ReadRam vram (bits [b0] & (i_ab `slice` (9,0))) -- BUG#6, hacked vram read addr was wrong
        ReadRom ram i_ab

      -- sometimes it can be the cpu_data out, but we're not modelling the cpu here
      let sync_bus_db = rams_data_out

      let i_db = sync_bus_db
      let i_flip = b0
      let i_wr2_l = b0

      inputs <- do
        pure V.Inputs { i_hcnt, i_vcnt, i_ab, i_db
                      , i_hblank, i_vblank, i_flip, i_wr2_l, ena_6 }

      outputs <-
        pacman_video sprite_ram roms rams regs inputs

      drivePixel inputs outputs


drivePixel :: V.Inputs -> V.Outputs -> Eff ()
drivePixel V.Inputs{i_vblank,i_hcnt,i_vcnt} V.Outputs{o_red,o_green,o_blue} = do

  --let colByte = o_red & o_green & o_blue -- BUG#3
  let colByte = o_blue & o_green & o_red
  rgb <- decodeAsRGB colByte

  x <- Minus (eSized 9 495) i_vcnt -- 272..495 --> 223..0 (flips left/right)

  if_ (not i_vblank) $ do

    partA <- Less i_hcnt (eSized 9 153) -- 128..152 (25)
    partB <- Less (eSized 9 248) i_hcnt -- 249..511 (263)

    if_ partA $ do
      y <- Plus i_hcnt (eSized 9 135) -- 128..152 --> 263..287
      let xy = XY { x, y }
      SetPixel xy rgb

    if_ partB $ do
      y <- Minus i_hcnt (eSized 9 249) -- 249..511 --> 0..262
      let xy = XY { x, y }
      SetPixel xy rgb
