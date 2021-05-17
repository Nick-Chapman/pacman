
{-# LANGUAGE RecordWildCards #-}

module PacVideo_Vhdl (theVideoSystem) where

import Prelude hiding (not,and,or,(<=))
import System
import Value

----------------------------------------------------------------------
-- | system connect: 'pacman.vhd'

theVideoSystem :: System
theVideoSystem = do
  withRoms $ \roms -> do
  withRams $ \rams@Rams{ram} -> do
  withRegisters $ \registers -> do
  withVideoTimingRegs $ \vtRegs -> do
  DeclareRom (RomSpec { path = "dump", size = 2048 }) $ \dump -> do
  let x = 224 -- (28*8)
  let y = 288 -- (36*8)
  let ss = defaultScreenSpec { sf = 3, size = XY { x, y } }
  FrameEffect ss $ do
   loadDump dump rams
   --loadSpriteDump rams

   Repeat 384 $ do -- TODO: needs to be 384*264 (every 1/60s frame) !

    let ena_6 :: E Bit = b1

    let VideoTimingRegs{..} = vtRegs
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
      ReadRam ram i_ab

    -- sometimes it can be the cpu_data out, but we're not modelling the cpu here
    let sync_bus_db = rams_data_out

    let i_db = sync_bus_db
    let i_flip = b0
    let i_wr2_l = b0

    inputs <- do
      pure Inputs { i_hcnt, i_vcnt, i_ab, i_db
                  , i_hblank, i_vblank, i_flip, i_wr2_l, ena_6 }

    outputs <-
      pacman_video roms rams registers inputs

    drivePixel inputs outputs


loadDump :: RomId -> Rams -> Eff () -- TODO: dev idea. read direct from dump
loadDump dump Rams{ram} = do
  sequence_ [ do
                b <- ReadRomByte dump (eSized 11 i)
                --let b = eSized 8 1 -- pick a specific tile for debug
                WriteRam ram (eSized 11 i) b
            | i <- [0..2047]]


{-loadSpriteDump :: Rams -> Eff ()
loadSpriteDump Rams{sprite_ram,sprite_xy_ram} = do
  sequence_ [ do WriteRam sprite_ram (eSized 4 i) (eSized 8 b)
            | (i,b) <- zip [0..] info
            ]
  sequence_ [ do WriteRam sprite_xy_ram (eSized 4 i) (eSized 8 b)
            | (i,b) <- zip [0..] xys
            ]
  where
    info = [ 0x00, 0x00, 0x94, 0x01, 0x94, 0x03, 0x8c, 0x05
           , 0x8c, 0x07, 0xb8, 0x09, 0xfc, 0x00, 0x00, 0x00 ]

    xys = [ 0x00, 0x00, 0xb6, 0x8c, 0xa1, 0xa4, 0x97, 0x8e
          , 0x77, 0x8e, 0xa3, 0x2c, 0x07, 0x08, 0x00, 0x00 ]
-}


{-
  Display is rotated 90 degrees, so:
    vcnt, maps to the X position on the screen (flipped)
    hcnt, maps to the Y position on the screen

X: vcnt (9 bits), varies more slowly
  runs:   248 (0x0F8) .. 511 (0x1FF) -- 264 ticks
  vblank: 495 (0x1EF) .. 271 (0x10F) --  40 ticks

  pixels: 272..495 (#224)

Y: hcnt (9 bits), varies more quickly
  runs:   128 (0x080) .. 511 (0x1FF) -- 384 ticks
  blank:  143 (0x08F) .. 239 (0x0EF) --  96 ticks

  pixels: 128..143, 240..511 (#288)
-}

drivePixel :: Inputs -> Outputs -> Eff ()
drivePixel Inputs{i_vblank,i_hcnt,i_vcnt} Outputs{o_red,o_green,o_blue} = do

  -- hblank seems off (by 8?) regarding which pixels to display ?!

  --let colByte = o_red & o_green & o_blue -- BUG#3
  let colByte = o_blue & o_green & o_red
  rgb <- decodeAsRGB colByte

  x <- Minus (eSized 9 495) i_vcnt -- 272..495 --> 223..0 (flips left/right)

  if_ (not i_vblank) $ do

    partA <- Less i_hcnt (eSized 9 152) -- 128..151 (24)
    partB <- Less (eSized 9 247) i_hcnt -- 248..511 (264)

    if_ partA $ do
      y <- Plus i_hcnt (eSized 9 136) -- 128..151 --> 264..287
      let xy = XY { x, y }
      SetPixel xy rgb

    if_ partB $ do
      y <- Minus i_hcnt (eSized 9 248) -- 248..511 --> 0..263
      let xy = XY { x, y }
      SetPixel xy rgb


decodeAsRGB :: E Nat -> Eff (RGB (E Nat))
decodeAsRGB w = do
  let
    bit :: Int -> Int -> Eff (E Nat)
    bit i v = do
      let c = w `index` i
      Mux c YN{ yes = nat8 v, no = nat8 0 }
  r <- do
    x <- bit 0 0x21
    y <- bit 1 0x47
    z <- bit 2 0x97
    add3 x y z
  g <- do
    x <- bit 3 0x21
    y <- bit 4 0x47
    z <- bit 5 0x97
    add3 x y z
  b <- do
    x <- bit 6 0x51
    y <- bit 7 0xAE
    let z = nat8 0
    add3 x y z
  pure RGB { r, g, b }
  where
    add3 a b c = do ab <- Plus a b; Plus ab c
    nat8 = eSized 8

----------------------------------------------------------------------
-- | video timing logic: 'pacman.vhd'

data VideoTimingRegs = VideoTimingRegs
  { hcnt :: Reg B9
  , vcnt :: Reg B9
  , hblank :: Reg Bit
  , vblank :: Reg Bit
  }

withVideoTimingRegs :: (VideoTimingRegs -> System) -> System
withVideoTimingRegs f = do
  DeclareRegi "hcnt" (sizedNat 9 0x080) $ \hcnt -> do
  DeclareRegi "vcnt" (sizedNat 9 0x0F8) $ \vcnt -> do
  DeclareReg1 "hblank" $ \hblank -> do
  DeclareReg1i "vblank" B1 $ \vblank -> do
  f VideoTimingRegs { hcnt, vcnt, hblank, vblank }

video_timing :: VideoTimingRegs -> E Bit -> Eff ()
video_timing VideoTimingRegs{..} ena_6 = do

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


----------------------------------------------------------------------

-- aliases for specific bit widths (if I ever try for length-indexed vectors)
type B2 = [Bit]
type B3 = [Bit]
type B4 = [Bit]
type B5 = [Bit]
type B8 = [Bit]
type B9 = [Bit]
type B12 = [Bit]

----------------------------------------------------------------------

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

data Roms = Roms
  { col_rom_7f :: RomId -- colours
  , col_rom_4a :: RomId -- palettes
  , char_rom_5e :: RomId -- tiles
  , char_rom_5f :: RomId -- sprites
  }

withRoms :: (Roms -> System) -> System
withRoms f = do
  declareRom "roms/82s123.7f" 32 $ \col_rom_7f -> do
  declareRom "roms/82s126.4a" 256 $ \col_rom_4a -> do
  declareRom "roms/pacman.5e" 4096 $ \char_rom_5e -> do
  declareRom "roms/pacman.5f" 4096 $ \char_rom_5f -> do
  f Roms { col_rom_7f , col_rom_4a , char_rom_5e , char_rom_5f }
    where
      declareRom path size f = do DeclareRom (RomSpec { path, size }) $ f

data Rams = Rams
  { sprite_xy_ram :: RamId
  , ram :: RamId
  }

withRams :: (Rams -> System) -> System
withRams f = do
  DeclareRam 16 $ \sprite_xy_ram -> do
  DeclareRam 4096 $ \ram -> do
  f Rams { sprite_xy_ram, ram }

data Registers = Registers
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
  , vout_obj_on_t1 :: Reg Bit
  , video_out :: Reg B8
  }

withRegisters :: (Registers -> System) -> System
withRegisters f = do
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
  DeclareReg1 "vout_obj_on_t1" $ \vout_obj_on_t1 -> do
  DeclareReg "video_out" (Size 8) $ \video_out -> do
    f Registers
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
      , vout_obj_on_t1
      , video_out
      }

----------------------------------------------------------------------
-- | main video decode logic: 'pacman_video.vhd'

pacman_video :: Roms -> Rams -> Registers -> Inputs -> Eff Outputs
pacman_video Roms{..} Rams{..} Registers{..} Inputs{..} = do

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
  vout_obj_on_t1' <- GetReg vout_obj_on_t1
  vout_yflip' <- GetReg vout_yflip

  -- Seems the write into the sprit ram comes via here; let's ignore that
  -- sprite_xy_ram_wen :: E Bit <- not i_wr2_l `and` ena_6

  -- BUG#5, read from sprite_xy_ram should be registered
  -- sprite_xy_ram
  do
    dpo <- ReadRam sprite_xy_ram (i_ab `slice` (3,0))
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
      ca5 <- (char_sum_reg' `index` 3) `xor` xflip
      Mux (char_hblank_reg' `isB` B0)
        YN { yes = db_reg' `slice` (1,0)
           , no = bits [ca5, i_hcnt `index` 3]
           }

    ca3 <- (i_hcnt `index` 2) `xor` yflip
    ca2 <- (char_sum_reg' `index` 2) `xor` xflip
    ca1 <- (char_sum_reg' `index` 1) `xor` xflip
    ca0 <- (char_sum_reg' `index` 0) `xor` xflip
    pure (ca_11_6 & ca54 & bits [ca3,ca2,ca1,ca0])

  -- char roms
  char_rom_5e_dout :: E B8 <- do
    read <- ReadRomByte char_rom_5e ca
    Mux ena_6 YN{ yes = read, no = byte0 }

  char_rom_5f_dout :: E B8 <- do
    read <- ReadRomByte char_rom_5f ca
    Mux ena_6 YN{ yes = read, no = byte0 }

  -- p_char_data_mux
  cd :: E B8 <- do
    Mux char_hblank_reg' YN { no = char_rom_5e_dout
                            , yes = char_rom_5f_dout }

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
    ReadRomByte col_rom_4a col_rom_addr

  -- p_cntr_ld
  cntr_ld <- do
    ena <- i_hcnt `slice` (3,0) `isV` [B0,B1,B1,B1]
    c <- vout_hblank' `or` not vout_obj_on'
    ena `and` c

  do -- p_ra_cnt
    if_ ena_6 $ do
      Ite cntr_ld $ \case
        B1 -> do
          SetReg ra dr
        B0 -> do
          ra1 <- Plus ra' byte1
          SetReg ra ra1

  -- TODO: The sprite_ram stuff needs reworking, for now, disable
  sprite_ram_op :: E B4 <- do
    pure nibble0
{-
  sprite_ram_addr :: E B12 <- do
    pure $ bits [b0,b0,b0,b0] & ra'

  -- dont care about the write side of the sprite ram
  -- let _ = (sprite_ram_ip, sprite_ram_addr_t1, vout_obj_on_t1)

  -- The original was a ram of 32 nibbles. 5 bit address, 4 bit data-out
  -- Here I use a ram of 16 bytes, and select either the hi/lo nibble
    let a = sprite_ram_addr `slice` (4,1)
    b <- do
      read <- ReadRam sprite_ram a
      Mux ena_6 YN{ yes = read, no = nibble0 }
    --Trace "sprite_ram (a,b)" [a,b]
    let s = sprite_ram_addr `index` 0
    -- TODO: which way around are the nibbles addressed by the LSB ?
    -- when I actually load my dumped sprite data I might be able to tell!
    Mux s YN { yes = b `slice` (7,4), no = b `slice` (3,0) }
-}

  -- p_sprite_ram_op_comb
  sprite_ram_reg :: E B4 <- do
    Mux vout_obj_on_t1' YN { yes = sprite_ram_op, no = nibble0 }

  -- p_video_op_sel_comb
  video_op_sel :: E Bit <- do
    not <$> (sprite_ram_reg `isV` [B0,B0,B0,B0])

  do -- p_sprite_ram_ip_reg
    if_ ena_6 $ do
      vout_obj_on_t1 <= vout_obj_on'

  -- Dont need to drive the write side of the sprite RAM
  -- p_sprite_ram_ip_comb
  -- sprite_ram_ip :: E B4 <- ...

  -- p_video_op_comb
  final_col :: E B4 <- do
    cond <- vout_hblank' `or` i_vblank
    v <- Mux video_op_sel YN { yes = sprite_ram_reg
                             , no = lut_4a `slice` (3,0) }
    Mux cond YN { yes = nibble0, no = v }

  -- col_rom_7f
  lut_7f :: E B8 <- do
    ReadRomByte col_rom_7f final_col

  do --p_final_reg
    if_ ena_6 $ do
      video_out <= lut_7f

  -- assign outputs
  do
    let o_blue = video_out' `slice` (7,6)
    let o_green = video_out' `slice` (5,3)
    let o_red = video_out' `slice` (2,0)
    pure Outputs { o_red, o_green, o_blue }


----------------------------------------------------------------------
-- | vram address custom ic: 'pacman_vram_addr.vhd'

pacman_vram_addr :: E B9 -> E B8 -> Eff (E B12)
pacman_vram_addr h v = do

  let flip = b0 -- TODO: connect to the emulator keyboard

  let [h256_l,h128,h64,h32,h16,h8,h4,h2,h1] = split h
  let [       v128,v64,v32,v16,v8,v4,v2,v1] = split v

  let _ = (h2,h1,v4,v2,v1) -- unused

  -- p_vp_comb
  v128p   <- flip `xor` v128
  v64p    <- flip `xor` v64
  v32p    <- flip `xor` v32
  v16p    <- flip `xor` v16
  v8p     <- flip `xor` v8

  -- p_hp_comb
  h128p   <- flip `xor` h128
  h64p    <- flip `xor` h64
  h32p    <- flip `xor` h32
  h16p    <- flip `xor` h16
  h8p     <- flip `xor` h8

  -- p_sel
  sel <- do
    d1 <- h32 `xor` h16
    d2 <- h32 `xor` h64
    d12 <- d1 `or` d2
    pure $ not d12  -- BUG#2, missing a not here!

  -- U6
  y157_11_8 <- do
    let s = sel
    let g = b0
    let a = bits [b1,b1,b1,b1]
    let b = bits [b0,h4,h64p,h64p]
    x74_157 s g a b

  -- U5
  y157_7_4 <- do
    let s = sel
    let g = b0
    let a = bits [b1,b1,b1,b1]
    let b = bits [h64p,h64p,h8p,v128p]
    x74_157 s g a b

  -- U4
  y157_3_0 <- do
    let s = sel
    let g = b0
    let a = bits [h64,h32,h16,h4]
    let b = bits [v64p,v32p,v16p,v8p]
    x74_157 s g a b

  let y157 = y157_11_8 & y157_7_4 & y157_3_0

  -- U3
  ab_11_8 <- do
    let s = h256_l
    let a = y157 `slice` (11,8)
    let b = bits [b0,h4,v128p,v64p]
    x74_257 s a b

  -- U2
  ab_7_4 <- do
    let s = h256_l
    let a = y157 `slice` (7,4)
    let b = bits [v32p,v16p,v8p,h128p]
    x74_257 s a b

 -- U1
  ab_3_0 <- do
    let s = h256_l
    let a = y157 `slice` (3,0)
    let b = bits [h64p,h32p,h16p,h8p]
    x74_257 s a b

  let ab = ab_11_8 & ab_7_4 & ab_3_0
  pure ab


x74_157 :: E Bit -> E Bit -> E B4 -> E B4 -> Eff (E B4)
x74_157 s g a b = do
  res <- Mux s YN {yes = b, no = a}
  Mux g YN{ yes = nibble0, no = res }

x74_257 :: E Bit -> E B4 -> E B4 -> Eff (E B4)
x74_257 s a b = do
  -- Uses a register in the simulation model. Can we forget this?
  Mux s YN { yes = b, no = a }

----------------------------------------------------------------------
-- literals

b0,b1 :: E Bit
b0 = eLit 1 B0
b1 = eLit 1 B1

byte0, byte1 :: E Nat
byte0 = eSized (Size 8) 0
byte1 = eSized (Size 8) 1

nibble0 :: E Nat
nibble0 = eSized (Size 4) 0

----------------------------------------------------------------------
-- utils

bits :: [E Bit] -> E [Bit]
bits = combine

slice :: E [Bit] -> (Int,Int) -> E [Bit]
slice e (high,low) =
  if high < low then error "slice:high<low" else do
    -- BUG#4, slice did accidental reverse (with "reverse" missing)
    combine (reverse [ index e i | i <- [low..high] ])

(&) :: E [Bit] -> E [Bit] -> E [Bit]
(&) e1 e2 = combine (split e1 ++ split e2)

-- constant equality test for vectors
-- TODO: better to use a bitwise xor with a constant
isV :: E [Bit] -> [Bit] -> Eff (E Bit)
isV es bs =
  allBitsSet (combine [ isB e b | (e,b) <- zipChecked (split es) bs ])

isB :: E Bit -> Bit -> E Bit
isB e = \case
  B0 -> not e
  B1 -> e

zipChecked :: [a] -> [b] -> [(a,b)]
zipChecked xs ys = do
  let xn = length xs
  let yn = length ys
  if xn /= yn then error (show ("zipChecked",xn,yn)) else
    zip xs ys

allBitsSet :: E [Bit] -> Eff (E Bit) -- TODO: primitive?
allBitsSet xs = IsZero (notV xs)

notV :: E [Bit] -> E [Bit]
notV = combine . map not . split -- TODO: primitive

----------------------------------------------------------------------
-- gates

not :: E Bit -> E Bit
not = eNot

and :: E Bit -> E Bit -> Eff (E Bit)
and = And

or :: E Bit -> E Bit -> Eff (E Bit)
or x y = do
  w <- and (not x) (not y)
  pure $ not w

xor :: E Bit -> E Bit -> Eff (E Bit)
xor x y = do
  w1 <- And x (not y)
  w2 <- And (not x) y
  w1 `or` w2

----------------------------------------------------------------------
-- other opps

(<=) :: Show a => Reg a -> E a -> Eff ()
(<=) = SetReg
infix 0 <=

if_ :: E Bit -> Eff () -> Eff ()
if_ e then_ = do
  Ite e $ \case
    B1 -> then_
    B0 -> pure ()
