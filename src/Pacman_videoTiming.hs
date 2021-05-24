{-# LANGUAGE RecordWildCards #-}

-- | Video timing logic.
-- | Derived from a fragment of: ../vhdl/pacman.vhd

module Pacman_videoTiming (withRegs, Regs(..), video_timing) where

import MiscVHDL (B9,b0,b1,if_,(<=),isV,notV)
import Prelude hiding (not,and,or,(<=))
import System (System(..),Eff(..),E,Reg,eSized)
import Value (Bit(..),sizedNat,Size(..))

data Regs = Regs
  { hcnt :: Reg B9
  , vcnt :: Reg B9
  , hblank :: Reg Bit
  , vblank :: Reg Bit
  }

withRegs :: (Regs -> System) -> System
withRegs f = do
  DeclareRegi "hcnt" (sizedNat 9 0x080) $ \hcnt -> do
  DeclareRegi "vcnt" (sizedNat 9 0x0F8) $ \vcnt -> do
  DeclareReg1 "hblank" $ \hblank -> do
  DeclareReg1i "vblank" B1 $ \vblank -> do
  f Regs { hcnt, vcnt, hblank, vblank }

video_timing :: Regs -> E Bit -> Eff ()
video_timing Regs{..} ena_6 = do

  hcnt' <- GetReg hcnt
  vcnt' <- GetReg vcnt

  -- no need to define vsync stuff
  do_hsync <- do
    hcnt' `isV` sizedNat (Size 9) 0xAF

  do -- p_hvcnt
    if_ ena_6 $ do
      hcarry <- allBitsSet hcnt'

      Ite hcarry $ \case
        B1 -> hcnt <= eSized (Size 9) 0x080
        B0 -> do
          hcnt1 <- Plus hcnt' (eSized 9 1)
          hcnt <= hcnt1

      vcarry <- allBitsSet vcnt'

      if_ do_hsync $ do
        Ite vcarry $ \case
          B1 -> do
            vcnt <= eSized (Size 9) 0xF8
          B0 -> do
            vcnt1 <- Plus vcnt' (eSized 9 1)
            vcnt <= vcnt1

  do -- p_sync
    if_ ena_6 $ do
      do
        c1 <- hcnt' `isV` sizedNat 9 0x08F
        Ite c1 $ \case
          B1 -> hblank <= b1
          B0 -> do
            c0 <- hcnt' `isV` sizedNat 9 0x0EF
            Ite c0 $ \case
              B1 -> hblank <= b0
              B0 -> pure ()
      do
        if_ do_hsync $ do
          c1 <- vcnt' `isV` sizedNat 9 0x1EF
          Ite c1 $ \case
            B1 -> do vblank <= b1
            B0 -> do
              c0 <- vcnt' `isV` sizedNat 9 0x10F
              Ite c0 $ \case
                B1 -> do vblank <= b0
                B0 -> do pure ()

allBitsSet :: E [Bit] -> Eff (E Bit) -- TODO: primitive?
allBitsSet xs = IsZero (notV xs)
