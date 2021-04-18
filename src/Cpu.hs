
module Cpu (
  Cpu(..),Reg16(..),Reg(..),Flag(..),
  init,
  get16,set16,
  get,set,
  flagBitPos,
--  getFlag,setFlag,
--  kindOfMap,
  ) where

import Prelude hiding (init)
import Phase (Addr,Byte,Bit)

data Reg = PCH | PCL | A | B | C | D | E | I
  | Flags
  deriving (Eq,Ord,Show)

data Reg16 = SP | HL
  deriving (Eq,Show)

data Flag = FlagS | FlagZ | FlagA | FlagP | FlagCY
  deriving (Eq,Show)

data Cpu p = Cpu
  { pch :: Byte p
  , pcl :: Byte p
  , sp :: Addr p
  , hl :: Addr p
  , regA :: Byte p
  , regB :: Byte p
  , regC :: Byte p
  , regD :: Byte p
  , regE :: Byte p
  , regF :: Byte p -- TODO: amalgamated flags
  , regI :: Byte p -- TODO: interrupt vector
{-  , flagS :: Bit p
  , flagZ :: Bit p
  , flagA :: Bit p
  , flagP :: Bit p
  , flagCY :: Bit p -}
  }

instance (Show (Addr p), Show (Bit p), Show (Byte p)) => Show (Cpu p) where
  show Cpu{pch,pcl
          ,sp,hl
          ,regA,regB,regC,regD,regE,regF,regI
          --,flagS,flagZ,flagA,flagP,flagCY
          } = unwords
    [ name ++ ": " ++ v
    | (name,v) <-
      [ ("PC", show pch ++ show pcl ++ ",")
      , ("AF", show regA ++ show regF ++ ",")
      , ("BC", show regB ++ show regC ++ ",")
      , ("DE", show regD ++ show regE  ++ ",")
      , ("HL", show hl ++ ",")
      , ("SP", show sp ++ ",")
      , ("I", show regI ++ ",")
--      , ("SZAPY", show flagS <> show flagZ <> show flagA <> show flagP <> show flagCY)
      ]
    ]


init :: Addr p -> Addr p -> Byte p -> Byte p -> Bit p -> Cpu p
init addr0 aFF b bFF _bit0 =
  Cpu { pch = b, pcl = b
      , sp = aFF
      , hl = addr0
      , regA = bFF, regF = bFF
      , regB = b, regC = b, regD = b, regE = b
      , regI = b
--      , flagS = bit0, flagZ = bit0, flagA = bit0, flagP = bit0, flagCY = bit0
      }

{-
getFlag :: Cpu p -> Flag -> Bit p
getFlag Cpu{flagS,flagZ,flagA,flagP,flagCY} = \case
  FlagS -> flagS
  FlagZ -> flagZ
  FlagA -> flagA
  FlagP -> flagP
  FlagCY -> flagCY

setFlag :: Cpu p -> Flag -> Bit p -> Cpu p
setFlag cpu flag x = case flag of
  FlagS -> cpu { flagS = x }
  FlagZ -> cpu { flagZ = x }
  FlagA -> cpu { flagA = x }
  FlagP -> cpu { flagP = x }
  FlagCY -> cpu { flagCY = x }
-}

flagBitPos :: Flag -> Int
flagBitPos = \case
  FlagS -> 7
  FlagZ -> 6
  FlagA -> 4
  FlagP -> 2
  FlagCY -> 0

get :: Cpu p -> Reg -> Byte p
get Cpu{pch,pcl,regA,regB,regC,regD,regE,regI,regF} = \case
  PCH -> pch
  PCL -> pcl
  A -> regA
  B -> regB
  C -> regC
  D -> regD
  E -> regE
  Flags -> regF -- error "Cpu.get Flags"
  I -> regI

set :: Cpu p -> Reg -> Byte p -> Cpu p
set cpu r x = case r of
  PCH -> cpu { pch = x}
  PCL -> cpu { pcl = x }
  A -> cpu { regA = x }
  B -> cpu { regB = x }
  C -> cpu { regC = x }
  D -> cpu { regD = x }
  E -> cpu { regE = x }
  Flags -> cpu { regF = x } --error "Cpu.set Flags"
  I -> cpu { regI = x }


get16 :: Cpu p -> Reg16 -> Addr p
get16 Cpu{sp,hl} = \case
  SP -> sp
  HL -> hl

set16 :: Cpu p -> Reg16 -> Addr p -> Cpu p
set16 cpu rr a = case rr of
  SP -> cpu { sp = a }
  HL -> cpu { hl = a }


{-
kindOfMap :: (Addr a -> Addr b) -> (Byte a -> Byte b) -> (Bit a -> Bit b) -> Cpu a -> Cpu b
kindOfMap af f g = \case
  Cpu{pch,pcl
     ,sp,hl
     ,regA,regB,regC,regD,regE,regF,regI
--     ,flagS,flagZ,flagA,flagP,flagCY
     } ->
    Cpu { pch = f pch
        , pcl = f pcl
        , sp = af sp
        , hl = af hl
        , regA = f regA
        , regB = f regB
        , regC = f regC
        , regD = f regD
        , regE = f regE
        , regF = f regF
        , regI = f regI
{-        , flagS = g flagS
        , flagZ = g flagZ
        , flagA = g flagA
        , flagP = g flagP
        , flagCY = g flagCY -}
        }
-}
