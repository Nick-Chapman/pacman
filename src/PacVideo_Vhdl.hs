
{-# LANGUAGE RecordWildCards #-}

module PacVideo_Vhdl (theVideoSystem) where

import Prelude hiding (not,and,or,(<=))
import System
import Value


-- TODO: just noticed we seem to be missing the vram lookup..??!
-- hmm, maybe that is what the 'sprite_xy_ram' is for?
-- NO.
-- The vram lookup is external to the vide (in 'pacman.vhd')
-- and (via the sync_bus_db) it should enter on the i_db input.
--      I_DB          => sync_bus_db,

theVideoSystem :: System
theVideoSystem = do
  withRoms $ \roms -> do
  withRams $ \rams -> do
  withRegisters $ \registers -> do
  withVideoTimingRegs $ \vtRegs -> do
  DeclareRom (RomSpec { path = "dump", size = 2048 }) $ \dump -> do
  FrameEffect $ do
   loadDump dump rams
   runForallPixels registers vtRegs $ do

    -- TODO: need to repeat the following effect N times
    -- for N covers all pixels. (we can prob skip the vblank)
    -- Need a new basic Effect primitive to repeat
    -- with initial behaviour to write a program to do the repeat
    -- But we can also experiment with compile-time loop unrolling
    -- Expect that would compute an enourmous program.
    -- For best effect would have to track register values at copilr-time

    let ena_6 :: E Bit = b1 -- so run 6 clocks for each step

    video_timing vtRegs ena_6

    let VideoTimingRegs{..} = vtRegs
    i_hcnt <- GetReg hcnt -- TODO: idea: intro type for reg/last-value
    i_vcnt <- GetReg vcnt
    i_hblank <- GetReg hblank
    i_vblank <- GetReg vblank

    vram_addr_ab <- do
      pacman_vram_addr i_hcnt (i_vcnt `slice` (7,0))

    -- ignore the time when the CPU controls the bus.
    let i_ab = vram_addr_ab
    let i_flip = b0
    let i_wr2_l = b0

    --let i_db = byte0 -- TODO: NO -- this need to be the vram lookup
    i_db <- vram_lookup rams vram_addr_ab

    -- TODO: for testing, dont forget to set the vram from the dump
    -- and set the sprite ram from the bytes I recorded.

    inputs <- do
      pure Inputs { i_hcnt, i_vcnt, i_ab, i_db
                  , i_hblank, i_vblank, i_flip, i_wr2_l, ena_6 }

    outputs <-
      pacman_video roms rams registers inputs

    drivePixel inputs outputs


vram_lookup :: Rams -> E B12 -> Eff (E B8)
vram_lookup Rams{vram} a = do
  ReadRam vram (bits [b0] & (a `slice` (9,0)))

----------------------------------------------------------------------
loadDump :: RomId -> Rams -> Eff ()
loadDump dump Rams{vram} = do
  sequence_ [ do
                b <- ReadRomByte dump (eSized 11 i)
                WriteRam vram (eSized 11 i) b
            |
              i <- [0..2047]
            ]

----------------------------------------------------------------------

{-
  -- y: AF -> B0 (x ticks)... 1FF -> 80
  -- x: 1FF -> F8
-}
runForallPixels :: Registers -> VideoTimingRegs -> Eff () -> Eff ()
{-runForallPixels Registers{vout_yflip} VideoTimingRegs{vcnt,hcnt}  eff = do
  vout_yflip <= b0
  vcnt <= eSized 9 0x10F
  _eRepeat 224 $ do
    hcnt <= eSized 9 0xEF
    _eRepeat 288 $ do
      eff-}

runForallPixels Registers{} VideoTimingRegs{} eff = do
  --vcnt <= eSized 9 0x10F
  Repeat 10000 $ do
    eff



_eRepeat :: Int -> Eff() -> Eff ()
_eRepeat n e = sequence_ (replicate n e)


drivePixel :: Inputs -> Outputs -> Eff ()
drivePixel Inputs{i_hcnt,i_vcnt} Outputs{o_red,o_green,o_blue} = do
  let x = i_vcnt
  let y = i_hcnt
  let xy = XY {x, y}
  let colByte = o_red & o_green & o_blue
  --let colByte' = (combine . map not . split) colByte
  rgb <- decodeAsRGB colByte
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
  f Roms { col_rom_7f -- colours
         , col_rom_4a -- palettes
         , char_rom_5e -- tiles
         , char_rom_5f -- sprites
         }
    where
      declareRom path size f = do DeclareRom (RomSpec { path, size }) $ f

data Rams = Rams
  { sprite_xy_ram :: RamId -- 16 bytes
  , sprite_ram :: RamId -- 16 bytes
  , vram :: RamId
  }

withRams :: (Rams -> System) -> System
withRams f = do -- TODO: this rsm will be written by the CPU
  DeclareRam 16 $ \sprite_xy_ram -> do
  DeclareRam 16 $ \sprite_ram -> do
  DeclareRam 2048 $ \vram -> do
  f Rams { sprite_xy_ram, sprite_ram, vram }

data Registers = Registers
  { char_sum_reg :: Reg B4
  , char_match_reg :: Reg Bit
  , char_hblank_reg :: Reg Bit
  , db_reg :: Reg B8
  , shift_regl :: Reg B4
  , shift_regu :: Reg B4
  , vout_obj_on :: Reg Bit
  , vout_yflip :: Reg Bit
  , vout_hblank :: Reg Bit
  , vout_db :: Reg B5
  , ra :: Reg B8
  , sprite_ram_addr_t1 :: Reg B12
  , vout_obj_on_t1 :: Reg Bit
  , lut_4a_t1      :: Reg B8
  , vout_hblank_t1 :: Reg Bit
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
  DeclareReg1 "vout_obj_on" $ \vout_obj_on -> do
  DeclareReg1 "vout_yflip" $ \vout_yflip -> do
  DeclareReg1 "vout_hblank" $ \vout_hblank -> do
  DeclareReg "vout_db" (Size 5) $ \vout_db -> do
  DeclareReg "ra" (Size 8) $ \ra -> do
  DeclareReg "sprite_ram_addr_t1" (Size 12) $ \sprite_ram_addr_t1 -> do
  DeclareReg1 "vout_obj_on_t1" $ \vout_obj_on_t1 -> do
  DeclareReg "lut_4a_t1" (Size 8) $ \lut_4a_t1 -> do
  DeclareReg1 "vout_hblank_t1" $ \vout_hblank_t1 -> do
  DeclareReg "video_out" (Size 8) $ \video_out -> do
    f Registers
      { char_sum_reg
      , char_match_reg
      , char_hblank_reg
      , db_reg
      , shift_regl
      , shift_regu
      , vout_obj_on
      , vout_yflip
      , vout_hblank
      , vout_db
      , ra
      , sprite_ram_addr_t1
      , vout_obj_on_t1
      , lut_4a_t1
      , vout_hblank_t1
      , video_out
      }


----------------------------------------------------------------------
-- 'pacman_video.vhd' code is derived from the VHDL simulation model

pacman_video :: Roms -> Rams -> Registers -> Inputs -> Eff Outputs
pacman_video Roms{..} Rams{..} Registers{..} Inputs{..} = do

  char_hblank_reg' <- GetReg char_hblank_reg
  char_match_reg' <- GetReg char_match_reg
  char_sum_reg' <- GetReg char_sum_reg
  db_reg' <- GetReg db_reg
  ra' <- GetReg ra
  shift_regl' <- GetReg shift_regl
  shift_regu' <- GetReg shift_regu
  video_out' <- GetReg video_out
  vout_db' <- GetReg vout_db
  vout_hblank' <- GetReg vout_hblank
  vout_obj_on' <- GetReg vout_obj_on
  vout_obj_on_t1' <- GetReg vout_obj_on_t1
  vout_yflip' <- GetReg vout_yflip

  -- Seems the write into the sprit ram comes via here; let's ignore that
  -- sprite_xy_ram_wen :: E Bit <- not i_wr2_l `and` ena_6

  -- sprite_xy_ram
  sprite_xy_ram_temp :: E B8 <-
    --ReadRam sprite_xy_ram i_ab -- TODO, the address for this read seems too wide!
    ReadRam sprite_xy_ram (i_ab `slice` (3,0))

  -- p_sprite_ram_comb
  dr :: E B8 <- do
    {-CaseBit i_hblank >>= \case
      B1 -> pure $ notV sprite_xy_ram_temp
      B0 -> pure $ bits (replicate 8 b1)-}
    Mux i_hblank
      YN { yes = notV sprite_xy_ram_temp
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
{-
  -- TODO HERE ** avoid all combinational uses of CaseBit & replace with Mux **
    char_hblank_reg <- GetReg char_hblank_reg
    CaseBit char_hblank_reg >>= \case
      B0 -> pure (i_flip, i_flip)
      B1 -> do
        db_reg <- GetReg db_reg
        pure (db_reg `index` 1, db_reg `index` 0)
-}
    --char_hblank_reg' <- GetReg char_hblank_reg
    --db_reg' <- GetReg db_reg
    xy <- Mux char_hblank_reg' YN { no = bits [i_flip, i_flip]
                                    , yes = db_reg' `slice` (1,0) }
    let [x,y] = split xy
    pure (x,y)

  -- p_char_addr_comb
  obj_on <- do
    --char_match_reg' <- GetReg char_match_reg
    char_match_reg' `or` (i_hcnt `index` 8)

  ca :: E B12 <- do
    --char_sum_reg' <- GetReg char_sum_reg
    --char_hblank_reg' <- GetReg char_hblank_reg
    --db_reg' <- GetReg db_reg
    let ca_11_6 = db_reg' `slice` (7,2)

    ca54 <- do
{-
      CaseBit (char_hblank_reg `isB` B0) >>= \case
        B1 -> pure $ db_reg `slice` (1,0)
        B0 -> do
          ca5 <- (char_sum_reg `index` 3) `xor` xflip
          pure $ bits [ca5, i_hcnt `index` 3]
-}
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
  -- TODO: must we model the enable?
  char_rom_5e_dout :: E B8 <- do
    --CaseBit ena_6 >>= \case B1 -> ReadRomByte char_rom_5e ca; B0 -> pure byte0
    read <- ReadRomByte char_rom_5e ca
    Mux ena_6 YN{ yes = read, no = byte0 }

  char_rom_5f_dout :: E B8 <- do
    --CaseBit ena_6 >>= \case B1 -> ReadRomByte char_rom_5f ca; B0 -> pure byte0
    read <- ReadRomByte char_rom_5f ca
    Mux ena_6 YN{ yes = read, no = byte0 }

  -- p_char_data_mux
  cd :: E B8 <- do
    --char_hblank_reg' <- GetReg char_hblank_reg
    {-CaseBit char_hblank_reg >>= \case
      B0 -> pure char_rom_5e_dout
      B1 -> pure char_rom_5f_dout-}
    Mux char_hblank_reg' YN { no = char_rom_5e_dout
                              , yes = char_rom_5f_dout }

  -- p_char_shift_comb
  (shift_sel,shift_op) <- do
    ip <- (i_hcnt `index` 0) `and` (i_hcnt `index` 0)
    --vout_yflip' <- GetReg vout_yflip
    --shift_regu' <- GetReg shift_regu
    --shift_regl' <- GetReg shift_regl
    {-CaseBit vout_yflip >>= \case
      B0 -> pure (bits [b1,ip], bits [shift_regu `index` 3, shift_regl `index` 3])
      B1 -> pure (bits [ip,b1], bits [shift_regu `index` 0, shift_regl `index` 0])-}
    shift_sel <- Mux vout_yflip' YN
      { no = bits [b1,ip]
      , yes = bits [ip,b1] }
    shift_op <- Mux vout_yflip' YN
      { no = bits [shift_regu' `index` 3, shift_regl' `index` 3]
      , yes = bits [shift_regu' `index` 0, shift_regl' `index` 0] }
    pure (shift_sel, shift_op)


  do -- p_char_shift
    --shift_regu' <- GetReg shift_regu
    --shift_regl' <- GetReg shift_regl
    if_ ena_6 $ do
      ite (shift_sel `index` 1) $ \case
        B0 -> do
          ite (shift_sel `index` 0) $ \case
            B0 -> do
              pure ()
            B1 -> do
              shift_regu <= bits [b0] & (shift_regu' `slice` (3,1))
              shift_regl <= bits [b0] & (shift_regl' `slice` (3,1))
        B1 -> do
          ite (shift_sel `index` 0) $ \case
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
    --vout_db' <- GetReg vout_db
    pure $ bits [b0] & vout_db' & shift_op

  -- col_rom_4a
  lut_4a <- do
    ReadRomByte col_rom_4a col_rom_addr

  -- p_cntr_ld
  cntr_ld <- do
    ena <- i_hcnt `slice` (3,0) `isV` [B0,B1,B1,B1]
    --vout_obj_on' <- GetReg vout_obj_on
    --vout_hblank' <- GetReg vout_hblank
    c <- vout_hblank' `or` not vout_obj_on'
    ena `and` c

  do -- p_ra_cnt
    if_ ena_6 $ do
      ite cntr_ld $ \case
        B1 -> do
          SetReg ra dr
        B0 -> do
          incremented <- do
            --ra' <- GetReg ra
            Plus ra' byte1
          SetReg ra incremented

  sprite_ram_addr :: E B12 <- do
    --ra' <- GetReg ra
    pure $ bits [b0,b0,b0,b0] & ra'

  -- dont care about the write side of the sprite ram
  -- let _ = (sprite_ram_ip, sprite_ram_addr_t1, vout_obj_on_t1)

  sprite_ram_op :: E B4 <- do
    let a = sprite_ram_addr `slice` (4,1)
    b <- do
{-
      CaseBit ena_6 >>= \case
        B1 -> ReadRam sprite_ram a
        B0 -> pure nibble0
-}
      read <- ReadRam sprite_ram a
      Mux ena_6 YN{ yes = read, no = nibble0 }
    let s = sprite_ram_addr `index` 0
    -- TODO: which way around are the nibbles addressed by the LSB ?
{-
    CaseBit s >>= \case
      B1 -> pure $ b `slice` (7,4)
      B0 -> pure $ b `slice` (3,0)
-}
    Mux s YN { yes = b `slice` (7,4), no = b `slice` (3,0) }

  -- p_sprite_ram_op_comb
  sprite_ram_reg :: E B4 <- do
    --vout_obj_on_t1' <- GetReg vout_obj_on_t1
    {-CaseBit vout_obj_on_t1 >>= \case
      B1 -> pure sprite_ram_op
      B0 -> pure nibble0-}
    Mux vout_obj_on_t1' YN { yes = sprite_ram_op, no = nibble0 }

  -- p_video_op_sel_comb
  video_op_sel :: E Bit <- do
    not <$> sprite_ram_reg `isV` [B0,B0,B0,B0]

  do -- p_sprite_ram_ip_reg
    --vout_obj_on' <- GetReg vout_obj_on
    --vout_hblank' <- GetReg vout_hblank
    if_ ena_6 $ do
      sprite_ram_addr_t1 <= sprite_ram_addr
      vout_obj_on_t1 <= vout_obj_on'
      vout_hblank_t1 <= vout_hblank'
      lut_4a_t1 <= lut_4a

  -- Dont need to drive the write side of the sprite RAM
  -- p_sprite_ram_ip_comb
  -- sprite_ram_ip :: E B4 <- ...

  -- p_video_op_comb
  final_col :: E B4 <- do
    --vout_hblank' <- GetReg vout_hblank
    cond <- vout_hblank' `or` i_vblank
    {-CaseBit cond >>= \case
      B1 -> pure nibble0
      B0 -> do
        CaseBit video_op_sel >>= \case
          B1 -> pure sprite_ram_reg
          B0 -> pure $ lut_4a `slice` (3,0)-}
    v <- Mux video_op_sel YN { yes = sprite_ram_reg
                                , no = lut_4a `slice` (3,0) }
    Mux cond YN { yes = nibble0 , no = v }

  -- col_rom_7f
  lut_7f :: E B8 <- do
    ReadRomByte col_rom_7f final_col

  do --p_final_reg
    if_ ena_6 $ do
      video_out <= lut_7f

  --  assign outputs
  do
    --video_out' <- GetReg video_out
    let o_blue = video_out' `slice` (7,6)
    let o_green = video_out' `slice` (5,3)
    let o_red = video_out' `slice` (2,0)
    pure Outputs { o_red, o_green, o_blue }


----------------------------------------------------------------------
-- literals

b0,b1 :: E Bit
b0 = eLit 1 B0
b1 = eLit 1 B1

byte0, byte1 :: E Nat
byte0 = eByte 0
byte1 = eByte 1

nibble0 :: E Nat
nibble0 = eSized (Size 4) 0

eByte :: Int -> E Nat
eByte = eSized (Size 8)

----------------------------------------------------------------------
-- utils

bits :: [E Bit] -> E [Bit]
bits = combine

slice :: E [Bit] -> (Int,Int) -> E [Bit]
slice e (high,low) =
  if high < low then error "slice:high<low" else do
    combine [ index e i | i <- [low..high] ]

(&) :: E [Bit] -> E [Bit] -> E [Bit]
(&) e1 e2 = combine (split e1 ++ split e2)

-- TODO: rename: combine/split --> bundle/unbundle (like  "clash")

-- constant equality test for vectors
isV :: E [Bit] -> [Bit] -> Eff (E Bit)
isV es bs =
  allBitsSet [ isB e b | (e,b) <- zipChecked (split es) bs ]

isB :: E Bit -> Bit -> E Bit
isB e = \case
  B0 -> not e
  B1 -> e

allBitsSet :: [E Bit] -> Eff (E Bit) -- TODO: should have a built-in oper for this
{-allBitsSet = \case
  [] -> pure b1
  x:xs -> do
    y <- allBitsSet xs
    x `and ` y-}
allBitsSet xs = isZero (combine (map not xs))

isZero :: E [Bit] -> Eff (E Bit)
isZero = IsZero

zipChecked :: [a] -> [b] -> [(a,b)]
zipChecked xs ys = do
  let xn = length xs
  let yn = length ys
  if xn /= yn then error (show ("zipChecked",xn,yn)) else
    zip xs ys


----------------------------------------------------------------------
--gates

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

notV :: E [Bit] -> E [Bit]
notV = combine . map not . split

----------------------------------------------------------------------
-- other opps

(<=) :: Show a => Reg a -> E a -> Eff ()
(<=) = SetReg
infix 0 <=

if_ :: E Bit -> Eff () -> Eff ()
if_ e then_ = do
  ite e $ \case
    B0 -> pure ()
    B1 -> then_


ite :: E Bit -> (Bit -> Eff ()) -> Eff ()
--ite sel f = CaseBit sel >>= f
ite = Ite

----------------------------------------------------------------------
-- aliases for specific bit widths (if I ever try for length-indexed vectors)

type B2 = [Bit]
type B3 = [Bit]
type B4 = [Bit]
type B5 = [Bit]
type B8 = [Bit]
type B9 = [Bit]
type B12 = [Bit]

{-increment :: Reg [Bit] -> Eff ()
increment reg = do
  reg' <- do
    reg <- GetReg reg
    Plus reg (eSized 9 1) -- TODO: compute this size from reg?
  reg <= reg'-}

----------------------------------------------------------------------

data VideoTimingRegs = VideoTimingRegs
  { hcnt :: Reg B9
  , vcnt :: Reg B9
  , hblank :: Reg Bit
  , vblank :: Reg Bit
--  , hsync :: Reg Bit
  }

withVideoTimingRegs :: (VideoTimingRegs -> System) -> System
withVideoTimingRegs f = do
  DeclareReg "hcnt" (Size 9) $ \hcnt -> do
  DeclareReg "vcnt" (Size 9) $ \vcnt -> do
  DeclareReg1 "hblank" $ \hblank -> do
  DeclareReg1 "vblank" $ \vblank -> do
--  DeclareReg1 $ \hsync -> do
  f VideoTimingRegs { hcnt, vcnt, hblank, vblank } --, hsync }

-- | stuff derived from 'pacman.vhd'
video_timing :: VideoTimingRegs -> E Bit -> Eff ()
video_timing VideoTimingRegs{..} ena_6 = do

  hcnt' <- GetReg hcnt
  vcnt' <- GetReg vcnt

  do_hsync <- do
    hcnt' `isV` sizedNat (Size 9) 0xAF

  do -- p_hvcnt
    if_ ena_6 $ do
      hcarry <- do
        --hcnt <- GetReg hcnt
        allBitsSet (split hcnt')

      ite hcarry $ \case
        B1 -> hcnt <= eSized (Size 9) 0x080
        B0 -> do
          --increment hcnt
          hcnt1 <- Plus hcnt' (eSized 9 1)
          hcnt <= hcnt1

      vcarry <- do
        --vcnt <- GetReg vcnt
        allBitsSet (split vcnt')

      if_ do_hsync $ do
        ite vcarry $ \case
          B1 -> do
            vcnt <= eSized (Size 9) 0xF8
          B0 -> do
            --increment vcnt
            vcnt1 <- Plus vcnt' (eSized 9 1)
            vcnt <= vcnt1

  do -- p_sync
    --hcnt <- GetReg hcnt
    if_ ena_6 $ do

      do
        c1 <- hcnt' `isV` sizedNat 9 0x08F
        ite c1 $ \case
          B1 -> hblank <= b1
          B0 -> do
            c0 <- hcnt' `isV` sizedNat 9 0x0EF
            ite c0 $ \case
              B1 -> hblank <= b0
              B0 -> pure ()

      do
{-        ite do_hsync $ \case
          B1 -> hsync <= b1
          B0 -> do
            c0 <- hcnt' `isV` sizedNat 9 0x0CF
            ite c0 $ \case
              B1 -> hsync <= b0
              B0 -> pure () -}

      if_ do_hsync $ do
        --vcnt <- GetReg vcnt
        c1 <- vcnt' `isV` sizedNat 9 0x1EF
        ite c1 $ \case
          B1 -> do vblank <= b1
          B0 -> do
            c0 <- vcnt' `isV` sizedNat 9 0x10F
            ite c0 $ \case
              B1 -> do vblank <= b0
              B0 -> do pure ()


----------------------------------------------------------------------

pacman_vram_addr :: E B8 -> E B9 -> Eff (E B12)
pacman_vram_addr h v = do

  let flip = b0 -- TODO: connect to the emulator keyboard

  let [h256_l,h128,h64,h32,h16,h8,h4,h2,h1] = split h
  let [       v128,v64,v32,v16,v8,v4,v2,v1] = split v

  let _ = (h2,h1,v4,v2,v1) -- unused

  -- TODO: check translation one more time before deleting the commented VHDL

{-
  p_vp_comb : process(FLIP, V8, V16, V32, V64, V128)
  begin
    v128p   <= FLIP xor V128;
    v64p    <= FLIP xor V64;
    v32p    <= FLIP xor V32;
    v16p    <= FLIP xor V16;
    v8p     <= FLIP xor V8;
  end process;
-}
  -- p_vp_comb
  v128p   <- flip `xor` v128
  v64p    <- flip `xor` v64
  v32p    <- flip `xor` v32
  v16p    <- flip `xor` v16
  v8p     <- flip `xor` v8
{-
  p_hp_comb : process(FLIP, H8, H16, H32, H64, H128)
  begin
    H128P   <= FLIP xor H128;
    H64P    <= FLIP xor H64;
    H32P    <= FLIP xor H32;
    H16P    <= FLIP xor H16;
    H8P     <= FLIP xor H8;
  end process;
-}
  -- p_hp_comb
  h128p   <- flip `xor` h128
  h64p    <- flip `xor` h64
  h32p    <- flip `xor` h32
  h16p    <- flip `xor` h16
  h8p     <- flip `xor` h8

{-
  p_sel     : process(H16, H32, H64)
  begin
    sel  <= not((H32 xor H16) or (H32 xor H64));
  end process;
-}
  -- p_sel
  sel <- do
    d1 <- h32 `xor` h16
    d2 <- h32 `xor` h64
    d1 `or` d2

{-
  U6        : X74_157
    port map(
      Y       => y157(11 downto 8),
      B(3)    => '0',
      B(2)    => H4,
      B(1)    => h64p,
      B(0)    => h64p,
      A       => "1111",
      G       => '0',
      S       => sel
      );
-}
  y157_11_8 <- do
    let s = sel
    let g = b0
    let a = bits [b1,b1,b1,b1]
    let b = bits [b0,h4,h64p,h64p]
    x74_157 s g a b

{-
  U5        : X74_157
    port map(
      Y       => y157(7 downto 4),
      B(3)    => h64p,
      B(2)    => h64p,
      B(1)    => h8p,
      B(0)    => v128p,
      A       => "1111",
      G       => '0',
      S       => sel
      );
-}
  y157_7_4 <- do
    let s = sel
    let g = b0
    let a = bits [b1,b1,b1,b1]
    let b = bits [h64p,h64p,h8p,v128p]
    x74_157 s g a b

{-
  U4        : X74_157
    port map(
      Y       => y157(3 downto 0),
      B(3)    => v64p,
      B(2)    => v32p,
      B(1)    => v16p,
      B(0)    => v8p,
      A(3)    => H64,
      A(2)    => H32,
      A(1)    => H16,
      A(0)    => H4,
      G       => '0',
      S       => sel
      );
-}
  y157_3_0 <- do
    let s = sel
    let g = b0
    let a = bits [h64,h32,h16,h4]
    let b = bits [v64p,v32p,v16p,v8p]
    x74_157 s g a b

  let y157 = y157_11_8 & y157_7_4 & y157_3_0

{-
  U3        : X74_257
    port map(
      Y       => AB(11 downto 8),
      B(3)    => '0',
      B(2)    => H4,
      B(1)    => v128p,
      B(0)    => v64p,
      A       => y157(11 downto 8),
      S       => H256_L
      );
-}
  ab_11_8 <- do
    let s = h256_l
    let a = y157 `slice` (11,8)
    let b = bits [b0,h4,v128p,v64p]
    x74_257 s a b

{-
  U2        : X74_257
    port map(
      Y       => AB(7 downto 4),
      B(3)    => v32p,
      B(2)    => v16p,
      B(1)    => v8p,
      B(0)    => h128p,
      A       => y157(7 downto 4),
      S       => H256_L
      );
-}
  ab_7_4 <- do
    let s = h256_l
    let a = y157 `slice` (7,4)
    let b = bits [v32p,v16p,v8p,h128p]
    x74_257 s a b

{-
  U1        : X74_257
    port map(
      Y       => AB(3 downto 0),
      B(3)    => h64p,
      B(2)    => h32p,
      B(1)    => h16p,
      B(0)    => h8p,
      A       => y157(3 downto 0),
      S       => H256_L
      );
-}
  ab_3_0 <- do
    let s = h256_l
    let a = y157 `slice` (3,0)
    let b = bits [h64p,h32p,h16p,h8p]
    x74_257 s a b

  let ab = ab_11_8 & ab_7_4 & ab_3_0
  pure ab



{-

entity X74_157 is
  port (
    Y       : out   std_logic_vector (3 downto 0);
    B       : in    std_logic_vector (3 downto 0);
    A       : in    std_logic_vector (3 downto 0);
    G       : in    std_logic;
    S       : in    std_logic
    );
end;
-}

x74_157 :: E Bit -> E Bit -> E B4 -> E B4 -> Eff (E B4)
x74_157 s g a b = do
  res <- Mux s YN {yes = b, no = a}
  Mux g YN{ yes = nibble0, no = res }

{-
architecture RTL of X74_157 is
begin
  p_y_comb      : process(S,G,A,B)
  begin
    for i in 0 to 3 loop
    -- quad 2 line to 1 line mux (true logic)
      if (G = '1') then
        Y(i) <= '0';
      else
        if (S = '0') then
          Y(i) <= A(i);
        else
          Y(i) <= B(i);
        end if;
      end if;
    end loop;
  end process;
end RTL;


entity X74_257 is
  port (
    Y       : out   std_logic_vector (3 downto 0);
    B       : in    std_logic_vector (3 downto 0);
    A       : in    std_logic_vector (3 downto 0);
    S       : in    std_logic
    );
end;
-}

x74_257 :: E Bit -> E B4 -> E B4 -> Eff (E B4)
x74_257 s a b = do
  -- Uses a register below. Can we forget this
  Mux s YN { yes = b, no = a }
{-

architecture RTL of X74_257 is
signal ab   : std_logic_vector (3 downto 0);
begin

  Y <= ab; -- no tristate
  p_ab     : process(S,A,B)
  begin
    for i in 0 to 3 loop
      if (S = '0') then
        AB(i) <= A(i);
      else
        AB(i) <= B(i);
      end if;
    end loop;
  end process;
end RTL;
-}
