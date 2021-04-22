
module Cpu (
  Cpu(..),Reg16(..),Reg(..),Flag(..),
  init,
  get16,set16,
  get,set,
  flagBitPos,
  ) where

import Prelude hiding (init)
import Phase (Addr,Byte,Bit)

data Reg = PCH | PCL | A | B | C | D | E | F | I
--  | A' | F'
  | B' | C' | D' | E'
  deriving (Eq,Ord,Show)

data Reg16 = SP | HL | HL'
  deriving (Eq,Show)

data Cpu p = Cpu
  { pch :: Byte p
  , pcl :: Byte p
  , sp :: Addr p
  , hl :: Addr p
  , hl' :: Addr p
  , rA :: Byte p
  , rB :: Byte p
  , rC :: Byte p
  , rD :: Byte p
  , rE :: Byte p
  , rF :: Byte p
  , rI :: Byte p
  , rB':: Byte p
  , rC':: Byte p
  , rD':: Byte p
  , rE':: Byte p
  }

instance (Show (Addr p), Show (Bit p), Show (Byte p)) => Show (Cpu p) where
  show Cpu{pch,pcl,sp,hl,rA,rB,rC,rD,rE,rF,rI} = unwords
    [ name ++ ": " ++ v
    | (name,v) <-
      [ ("PC", show pch ++ show pcl ++ ",")
      , ("AF", show rA ++ show rF ++ ",")
      , ("BC", show rB ++ show rC ++ ",")
      , ("DE", show rD ++ show rE  ++ ",")
      , ("HL", show hl ++ ",")
      , ("SP", show sp ++ ",")
      , ("I", show rI ++ ",")
      ]
    ]

init :: Addr p -> Addr p -> Byte p -> Byte p -> Cpu p
init addr0 aFF b bFF =
  Cpu { pch = b, pcl = b
      , sp = aFF
      , hl = addr0
      , hl'= addr0
      , rA = bFF, rF = bFF
      , rB = b, rC = b, rD = b, rE = b
      , rB'= b, rC'= b, rD'= b, rE'= b
      , rI = b
      }

data Flag = SF | ZF | YF| HF | XF | PF | NF | CF
  deriving (Eq,Show)

flagBitPos :: Flag -> Int
flagBitPos = \case
  SF -> 7
  ZF -> 6
  YF -> 5
  HF -> 4
  XF -> 3
  PF -> 2
  NF -> 1
  CF -> 0

get :: Cpu p -> Reg -> Byte p
get Cpu{pch,pcl,rA
       ,rB,rC,rD,rE
       ,rB',rC',rD',rE'
       ,rI,rF} = \case
  PCH -> pch
  PCL -> pcl
  A -> rA
  B -> rB
  C -> rC
  D -> rD
  E -> rE
  B'-> rB'
  C'-> rC'
  D'-> rD'
  E'-> rE'
  F -> rF
  I -> rI

set :: Cpu p -> Reg -> Byte p -> Cpu p
set cpu r x = case r of
  PCH -> cpu { pch = x}
  PCL -> cpu { pcl = x }
  A -> cpu { rA = x }
  B -> cpu { rB = x }
  C -> cpu { rC = x }
  D -> cpu { rD = x }
  E -> cpu { rE = x }
  B'-> cpu { rB'= x }
  C'-> cpu { rC'= x }
  D'-> cpu { rD'= x }
  E'-> cpu { rE'= x }
  F -> cpu { rF = x }
  I -> cpu { rI = x }

get16 :: Cpu p -> Reg16 -> Addr p
get16 Cpu{sp,hl,hl'} = \case
  SP -> sp
  HL -> hl
  HL' -> hl'

set16 :: Cpu p -> Reg16 -> Addr p -> Cpu p
set16 cpu rr a = case rr of
  SP -> cpu { sp = a }
  HL -> cpu { hl = a }
  HL' -> cpu { hl' = a }
