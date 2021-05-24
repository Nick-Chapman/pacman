
-- | Custom IC for vram address decoding
-- | Derived from: ../vhdl/pacman_vram_addr.vhd

module Pacman_vram_addr (pacman_vram_addr) where

import MiscVHDL (B4,B8,B9,B12,b0,b1,not,or,bits,(&),slice,nibble0)
import Prelude hiding (not,or)
import System (Eff(..),E,split)
import Value (Bit,YN(..))


x74_157 :: E Bit -> E Bit -> E B4 -> E B4 -> Eff (E B4)
x74_157 s g a b = do
  res <- Mux s YN {yes = b, no = a}
  Mux g YN{ yes = nibble0, no = res }


x74_257 :: E Bit -> E B4 -> E B4 -> Eff (E B4)
x74_257 s a b = do
  Mux s YN { yes = b, no = a }


pacman_vram_addr :: E B9 -> E B8 -> Eff (E B12)
pacman_vram_addr h v = do

  let flip = b0 -- TODO: connect to the emulator keyboard

  let [h256_l,h128,h64,h32,h16,h8,h4,h2,h1] = split h
  let [       v128,v64,v32,v16,v8,v4,v2,v1] = split v

  let _ = (h2,h1,v4,v2,v1) -- unused

  -- p_vp_comb
  v128p   <- flip `Xor` v128
  v64p    <- flip `Xor` v64
  v32p    <- flip `Xor` v32
  v16p    <- flip `Xor` v16
  v8p     <- flip `Xor` v8

  -- p_hp_comb
  h128p   <- flip `Xor` h128
  h64p    <- flip `Xor` h64
  h32p    <- flip `Xor` h32
  h16p    <- flip `Xor` h16
  h8p     <- flip `Xor` h8

  -- p_sel
  sel <- do
    d1 <- h32 `Xor` h16
    d2 <- h32 `Xor` h64
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
