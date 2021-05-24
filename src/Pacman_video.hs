{-# LANGUAGE RecordWildCards #-}

-- | Video decode logic.
-- | Derived from: ../vhdl/pacman_video.vhd

module Pacman_video (
  Inputs(..), Outputs(..), Regs, withRegs,
  pacman_video,
  ) where

import Prelude hiding (not,and,or,(<=))
import System (E,Reg,System(..),RamId,Eff(..),split,index)
import Value (Bit,YN(..),Bit(..),Size(..))

import MiscVHDL (B2,B3,B4,B5,B8,B9,B12,
                 slice,(<=),b0,b1,nibble0,byte1,isB,not,and,or,isV,notV,(&),bits,if_)

import Pacman_RomsAndRams (VideoRoms(..),RamDump(..))

data Inputs = Inputs
  { i_hcnt :: E B9
  , i_vcnt :: E B9
  , i_ab :: E B12
  , i_db :: E B8
  , i_hblank :: E Bit
  , i_vblank :: E Bit
  , i_flip :: E Bit
  , i_wr2_l :: E Bit
  , ena_6 :: E Bit
  }

data Outputs = Outputs
  { o_red   :: E B3
  , o_green :: E B3
  , o_blue  :: E B2
  }

data Regs = Regs
  { char_sum_reg :: Reg B4
  , char_match_reg :: Reg Bit
  , char_hblank_reg :: Reg Bit
  , db_reg :: Reg B8
  , shift_regl :: Reg B4
  , shift_regu :: Reg B4
  , sprite_xy_ram_temp :: Reg B8
  , vout_obj_on :: Reg Bit
  , vout_yflip :: Reg Bit
  , vout_hblank :: Reg Bit
  , vout_db :: Reg B5
  , ra :: Reg B8
  , video_out :: Reg B8

  , sprite_ram_addr_t1 :: Reg B12
  , vout_obj_on_t1 :: Reg Bit
  , vout_hblank_t1 :: Reg Bit
  , lut_4a_t1 :: Reg B8
  , char_rom_5e_dout :: Reg B8
  , char_rom_5f_dout :: Reg B8
--  , sprite_ram_reg :: Reg B4
  }

withRegs :: (Regs -> System) -> System
withRegs f = do
  DeclareReg "char_sum_reg" (Size 4) $ \char_sum_reg -> do
  DeclareReg1 "char_match_reg" $ \char_match_reg -> do
  DeclareReg1 "char_hblank_reg" $ \char_hblank_reg -> do
  DeclareReg "db_reg" (Size 8) $ \db_reg -> do
  DeclareReg "shift_regl" (Size 4) $ \shift_regl -> do
  DeclareReg "shift_regu" (Size 4) $ \shift_regu -> do
  DeclareReg "sprite_xy_ram_temp" (Size 8) $ \sprite_xy_ram_temp -> do
  DeclareReg1 "vout_obj_on" $ \vout_obj_on -> do
  DeclareReg1 "vout_yflip" $ \vout_yflip -> do
  DeclareReg1 "vout_hblank" $ \vout_hblank -> do
  DeclareReg "vout_db" (Size 5) $ \vout_db -> do
  DeclareReg "ra" (Size 8) $ \ra -> do
  DeclareReg "video_out" (Size 8) $ \video_out -> do
  DeclareReg "sprite_ram_addr_t1" (Size 12) $ \sprite_ram_addr_t1 -> do
  DeclareReg1 "vout_obj_on_t1" $ \vout_obj_on_t1 -> do
  DeclareReg1 "vout_hblank_t1" $ \vout_hblank_t1 -> do
  DeclareReg "lut_4a_t1" (Size 8) $ \lut_4a_t1 -> do
  DeclareReg "char_rom_5e_dout" (Size 8) $ \char_rom_5e_dout -> do
  DeclareReg "char_rom_5f_dout" (Size 8) $ \char_rom_5f_dout -> do
--  DeclareReg "sprite_ram_reg" (Size 4) $ \sprite_ram_reg -> do

    f Regs
      { char_sum_reg
      , char_match_reg
      , char_hblank_reg
      , db_reg
      , shift_regl
      , shift_regu
      , sprite_xy_ram_temp
      , vout_obj_on
      , vout_yflip
      , vout_hblank
      , vout_db
      , ra
      , video_out
      , sprite_ram_addr_t1
      , vout_obj_on_t1
      , vout_hblank_t1
      , lut_4a_t1
      , char_rom_5e_dout
      , char_rom_5f_dout
--      , sprite_ram_reg
      }


-- | main video decode logic: 'pacman_video.vhd'
pacman_video :: RamId -> VideoRoms -> RamDump -> Regs -> Inputs -> Eff Outputs
pacman_video sprite_ram VideoRoms{..} RamDump{..} Regs{..} Inputs{..} = do

  -- we suffix with prime for the current value of a register
  char_hblank_reg' <- GetReg char_hblank_reg
  char_match_reg' <- GetReg char_match_reg
  char_sum_reg' <- GetReg char_sum_reg
  db_reg' <- GetReg db_reg
  ra' <- GetReg ra
  shift_regl' <- GetReg shift_regl
  shift_regu' <- GetReg shift_regu
  sprite_xy_ram_temp' <- GetReg sprite_xy_ram_temp
  video_out' <- GetReg video_out
  vout_db' <- GetReg vout_db
  vout_hblank' <- GetReg vout_hblank
  vout_obj_on' <- GetReg vout_obj_on
  vout_yflip' <- GetReg vout_yflip
  sprite_ram_addr_t1' <- GetReg sprite_ram_addr_t1
  vout_obj_on_t1' <- GetReg vout_obj_on_t1
  vout_hblank_t1' <- GetReg vout_hblank_t1
  lut_4a_t1' <- GetReg lut_4a_t1
  char_rom_5e_dout' <- GetReg char_rom_5e_dout
  char_rom_5f_dout' <- GetReg char_rom_5f_dout
  --sprite_ram_reg' <- GetReg sprite_ram_reg

  -- Seems the write into the sprit ram comes via here; let's ignore that
  -- sprite_xy_ram_wen :: E Bit <- not i_wr2_l `and` ena_6

  -- BUG#5, read from sprite_xy_ram should be registered
  -- sprite_xy_ram
  do
    dpo <- ReadRom sprite_xy_ram (i_ab `slice` (3,0))
    sprite_xy_ram_temp <= dpo

  -- p_sprite_ram_comb
  dr :: E B8 <- do
    Mux i_hblank
      YN { yes = notV sprite_xy_ram_temp'
         , no = bits (replicate 8 b1) }

  do -- p_char_regs
    hIs011 <- (i_hcnt `slice` (2,0)) `isV` [B0,B1,B1]
    cond <- hIs011 `and` ena_6
    if_ cond $ do
      let inc = not i_hblank
      sum <- Plus (i_vcnt `slice` (7,0) & bits [b1]) (dr & bits [inc])
      match <- sum `slice` (8,5) `isV` [B1,B1,B1,B1]
      char_sum_reg <= sum `slice` (4,1)
      char_match_reg <= match
      char_hblank_reg <= i_hblank
      db_reg <= i_db

  -- p_flip_comb
  (xflip,yflip) <- do
    xy <- Mux char_hblank_reg' YN { no = bits [i_flip, i_flip]
                                  , yes = db_reg' `slice` (1,0) }
    let [x,y] = split xy
    pure (x,y)

  -- p_char_addr_comb
  obj_on <- do
    char_match_reg' `or` (i_hcnt `index` 8)

  ca :: E B12 <- do
    let ca_11_6 = db_reg' `slice` (7,2)

    ca54 <- do
      ca5 <- (char_sum_reg' `index` 3) `Xor` xflip
      Mux (char_hblank_reg' `isB` B0)
        YN { yes = db_reg' `slice` (1,0)
           , no = bits [ca5, i_hcnt `index` 3]
           }

    ca3 <- (i_hcnt `index` 2) `Xor` yflip
    ca2 <- (char_sum_reg' `index` 2) `Xor` xflip
    ca1 <- (char_sum_reg' `index` 1) `Xor` xflip
    ca0 <- (char_sum_reg' `index` 0) `Xor` xflip
    pure (ca_11_6 & ca54 & bits [ca3,ca2,ca1,ca0])

  -- char roms
  do
    if_ ena_6 $ do
      read <- ReadRom char_rom_5e ca
      char_rom_5e_dout <= read

      read <- ReadRom char_rom_5f ca
      char_rom_5f_dout <= read

  -- p_char_data_mux
  cd :: E B8 <- do
    Mux char_hblank_reg' YN { no = char_rom_5e_dout'
                            , yes = char_rom_5f_dout' }

  -- p_char_shift_comb
  (shift_sel,shift_op) <- do
    ip <- (i_hcnt `index` 0) `and` (i_hcnt `index` 1) -- BUG#1, 2nd index was 0
    shift_sel <-
      Mux vout_yflip' YN { no = bits [b1,ip]
                         , yes = bits [ip,b1] }
    shift_op <-
      Mux vout_yflip' YN { no = bits [shift_regu' `index` 3, shift_regl' `index` 3]
                         , yes = bits [shift_regu' `index` 0, shift_regl' `index` 0] }
    pure (shift_sel, shift_op)

  do -- p_char_shift
    if_ ena_6 $ do
      Ite (shift_sel `index` 1) $ \case
        B0 -> do
          Ite (shift_sel `index` 0) $ \case
            B0 -> do
              pure ()
            B1 -> do
              shift_regu <= bits [b0] & (shift_regu' `slice` (3,1))
              shift_regl <= bits [b0] & (shift_regl' `slice` (3,1))
        B1 -> do
          Ite (shift_sel `index` 0) $ \case
            B0 -> do
              shift_regu <= shift_regu' `slice` (2,0) & bits [b0]
              shift_regl <= shift_regl' `slice` (2,0) & bits [b0]
            B1 -> do
              shift_regu <= cd `slice` (7,4)
              shift_regl <= cd `slice` (3,0)

  -- p_video_out_reg
  do
    if_ ena_6 $ do
      cond <- (i_hcnt `slice` (2,0)) `isV` [B1,B1,B1]
      if_ cond $ do
        vout_obj_on <= obj_on
        vout_yflip <= yflip
        vout_hblank <= i_hblank
        vout_db <= i_db `slice` (4,0)

  -- p_lut_4a_comb
  col_rom_addr <- do
    pure $ bits [b0] & vout_db' & shift_op

  -- col_rom_4a
  lut_4a <- do
    ReadRom col_rom_4a col_rom_addr

  -- p_cntr_ld
  cntr_ld <- do
    ena <- i_hcnt `slice` (3,0) `isV` [B0,B1,B1,B1]
    c <- vout_hblank' `or` not vout_obj_on'
    ena `and` c

  do -- p_ra_cnt
    if_ ena_6 $ do
      Ite cntr_ld $ \case
        B1 -> do
          ra <= dr
        B0 -> do
          ra1 <- Plus ra' byte1
          ra <= ra1

  sprite_ram_addr :: E B12 <- do
    pure $ bits [b0,b0,b0,b0] & ra'

  -- u_sprite_ram (read side)
  sprite_ram_op :: E B4 <- do
    byte <- ReadRam sprite_ram sprite_ram_addr
    pure $ byte `slice` (3,0)

  -- p_sprite_ram_op_comb
  {-do
    v <- Mux vout_obj_on_t1' YN { yes = sprite_ram_op, no = nibble0 }
    sprite_ram_reg <= v -- BUG #8 -- wasn't registered; sometimes caused bar at bottom of pink ghost -}
  -- reinstate unregisterd sprite_ram_reg
  sprite_ram_reg' :: E B4 <- do
    Mux vout_obj_on_t1' YN { yes = sprite_ram_op, no = nibble0 }

  -- p_video_op_sel_comb
  video_op_sel :: E Bit <- do
    not <$> (sprite_ram_reg' `isV` [B0,B0,B0,B0])

  do -- p_sprite_ram_ip_reg
    if_ ena_6 $ do
      sprite_ram_addr_t1 <= sprite_ram_addr
      vout_obj_on_t1 <= vout_obj_on'
      vout_hblank_t1 <= vout_hblank'
      lut_4a_t1 <= lut_4a

  -- p_sprite_ram_ip_comb
  sprite_ram_ip :: E B4 <- do
    yes <- Mux video_op_sel YN { yes = sprite_ram_reg'
                               , no = lut_4a_t1' `slice` (3,0) }
    Mux vout_hblank_t1' YN { yes, no = nibble0 }

  -- u_sprite_ram (write side)
  do
    -- BUG #7 -- forgot the write-enable. Adding this fixes the replication bug.
    if_ vout_obj_on_t1' $
      WriteRam sprite_ram sprite_ram_addr_t1' (nibble0 & sprite_ram_ip)

  -- p_video_op_comb
  final_col :: E B4 <- do
    cond <- vout_hblank' `or` i_vblank
    v <- Mux video_op_sel YN { yes = sprite_ram_reg'
                             , no = lut_4a `slice` (3,0) }
    Mux cond YN { yes = nibble0, no = v }

  -- col_rom_7f
  lut_7f :: E B8 <- do
    ReadRom col_rom_7f final_col

  do --p_final_reg
    if_ ena_6 $ do
      video_out <= lut_7f

  -- assign outputs
  do
    let o_blue = video_out' `slice` (7,6)
    let o_green = video_out' `slice` (5,3)
    let o_red = video_out' `slice` (2,0)
    pure Outputs { o_red, o_green, o_blue }
