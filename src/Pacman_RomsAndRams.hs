
-- | Declare the roms & rams for a pacman system.

module Pacman_RomsAndRams (
  VideoRoms(..), withVideoRoms,
  RamDump(..), withRamDump,
  ) where

import System (System(DeclareRom),RomId,RomSpec(..))

data VideoRoms = VideoRoms
  { col_rom_7f :: RomId -- colours
  , col_rom_4a :: RomId -- palettes
  , char_rom_5e :: RomId -- tiles
  , char_rom_5f :: RomId -- sprites
  }

withVideoRoms :: (VideoRoms -> System) -> System
withVideoRoms f = do
  declareRom "roms/82s123.7f" 32 $ \col_rom_7f -> do
  declareRom "roms/82s126.4a" 256 $ \col_rom_4a -> do
  declareRom "roms/pacman.5e" 4096 $ \char_rom_5e -> do
  declareRom "roms/pacman.5f" 4096 $ \char_rom_5f -> do
  f VideoRoms { col_rom_7f , col_rom_4a , char_rom_5e , char_rom_5f }
    where
      declareRom path size f = do DeclareRom (RomSpec { path, size }) $ f

data RamDump = RamDump
  { ram :: RomId -- RamId
  , sprite_xy_ram :: RomId --RamId
  }

withRamDump :: String -> (RamDump -> System) -> System
withRamDump suf f = do
  --TODO: reinstate Ram, with explicit initialization from ram-dump
  --DeclareRam 16 $ \sprite_xy_ram -> do
  --DeclareRam 4096 $ \ram -> do
  DeclareRom (RomSpec { path = "dumps/ram."++suf, size = 4096 }) $ \ram -> do
  DeclareRom (RomSpec { path = "dumps/xy."++suf, size = 16 }) $ \sprite_xy_ram -> do
  f RamDump { sprite_xy_ram, ram }
