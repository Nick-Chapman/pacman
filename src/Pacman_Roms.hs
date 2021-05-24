
module Pacman_Roms (VideoRoms(..),withVideoRoms) where

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
