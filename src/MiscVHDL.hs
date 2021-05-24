
-- | Ops and types for systems converted from VHDL

module MiscVHDL (
  B2,B3,B4,B5,B8,B9,B12,
  b0,b1,not,or,and,slice,notV,(&),bits,if_,isV,isB,nibble0,byte1,(<=),
  ) where

import Prelude hiding (not,or,and,(<=))
import Value (Bit(..),Nat,Size(..))
import System (Reg,Eff(..),E,combine,index,split,eNot,eLit,eSized)

-- aliases for specific bit widths (if I ever try for length-indexed vectors)
type B2 = [Bit]
type B3 = [Bit]
type B4 = [Bit]
type B5 = [Bit]
type B8 = [Bit]
type B9 = [Bit]
type B12 = [Bit]

-- register assignenment operator
(<=) :: Show a => Reg a -> E a -> Eff ()
(<=) = SetReg
infix 0 <=

-- conditional effect
if_ :: E Bit -> Eff () -> Eff ()
if_ e then_ = do
  Ite e $ \case
    B1 -> then_
    B0 -> pure ()

-- vector manipulation
bits :: [E Bit] -> E [Bit]
bits = combine

-- vhdl slice operation
slice :: E [Bit] -> (Int,Int) -> E [Bit]
slice e (high,low) =
  if high < low then error "slice:high<low" else do
    -- BUG#4, slice did accidental reverse (with "reverse" missing)
    combine (reverse [ index e i | i <- [low..high] ])

-- vhdl concatenation op
(&) :: E [Bit] -> E [Bit] -> E [Bit]
(&) e1 e2 = combine (split e1 ++ split e2)

-- basic bit-level ops/gates
not :: E Bit -> E Bit
not = eNot

and :: E Bit -> E Bit -> Eff (E Bit)
and = And

or :: E Bit -> E Bit -> Eff (E Bit)
or x y = do
  w <- And (not x) (not y)
  pure $ not w

-- literals
b0,b1 :: E Bit
b0 = eLit 1 B0
b1 = eLit 1 B1

nibble0 :: E Nat
nibble0 = eSized (Size 4) 0

byte1 :: E Nat
byte1 = eSized (Size 8) 1

-- bitwise op
notV :: E [Bit] -> E [Bit]
notV = combine . map not . split -- TODO: primitive

-- comparison with literal
isV :: E [Bit] -> [Bit] -> Eff (E Bit)
isV es bs = do
  xs <- es `XorBitwise` combine (map (eLit 1) bs)
  IsZero xs

isB :: E Bit -> Bit -> E Bit
isB e = \case
  B0 -> not e
  B1 -> e
