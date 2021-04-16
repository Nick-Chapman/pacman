
module Types (Byte(..)) where

import Data.Word (Word8)
import Text.Printf (printf)

newtype Byte = Byte { unByte :: Word8 } deriving (Eq,Num,Integral,Real,Enum,Ord)
instance Show Byte where show = printf "%02X" . unByte
