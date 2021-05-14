module Ram (Ram,init,read,write) where

import Control.DeepSeq (NFData)
import Data.Map (Map)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Prelude hiding (init,read)
import qualified Data.Map.Strict as Map

data Ram = Ram { size :: Int, m :: Map Int Word8 }
  deriving (Generic,NFData)

init :: Int -> Ram
init size = Ram { size, m = Map.empty }

read :: Ram -> Int -> Word8
read Ram{size,m} a = if
  | a < 0 || a >= size -> error ("Ram.read: " ++ show a ++ ", size=" ++ show size)
  | otherwise ->
    Map.findWithDefault 0 a m

write :: Ram -> Int -> Word8 -> Ram
write ram@Ram{size,m} a b = if
  | a < 0 || a >= size -> error ("Ram.write: " ++ show a ++ ", size=" ++ show size)
  | otherwise ->
    ram { m = Map.insert a b m }
