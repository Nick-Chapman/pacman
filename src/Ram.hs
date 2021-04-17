
module Ram (Ram,init,read,write) where

import Byte (Byte(..))
import Data.Map (Map)
import Prelude hiding (init,read)
import qualified Data.Map.Strict as Map

data Ram = Ram { size :: Int, m :: Map Int Byte }

init :: Int -> Ram
init size = Ram { size, m = Map.empty }

read :: Ram -> Int -> Byte
read Ram{size,m} a = if
  | a < 0 || a >= size -> error (show ("Ram.read",a))
  | otherwise ->
    undefined size m a -- TODO: wait until reach
    --Map.findWithDefault (Byte 0) a m

write :: Ram -> Int -> Byte -> Ram
write ram@Ram{size,m} a b = if
  | a < 0 || a >= size -> error (show ("Ram.write",a))
  | otherwise ->
    undefined ram m a b --TODO: wait until reach
    --ram { m = Map.insert a b m }
