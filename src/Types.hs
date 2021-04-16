
module Types (Addr(..),Byte(..)) where

import Data.Word (Word8,Word16)
import Text.Printf (printf)

newtype Addr = Addr { unAddr :: Word16 } deriving (Eq,Ord,Num,Enum,Integral,Real)
instance Show Addr where show = printf "%04X" . unAddr

newtype Byte = Byte { unByte :: Word8 } deriving (Eq,Num,Integral,Real,Enum,Ord)
instance Show Byte where show = printf "%02X" . unByte
