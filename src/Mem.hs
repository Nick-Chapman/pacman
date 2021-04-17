
module Mem (Mem,Addr,init,read,write) where

import Prelude hiding (init,read)

import Addr (Addr(..))
import Byte (Byte(..))
--import Ram8k (Ram)
--import Rom (Rom)
--import qualified Ram8k (init,read,write)
--import qualified Rom (lookup)

import Data.Map (Map)
import qualified Data.Map.Strict as Map

data Mem = Mem { m :: Map Addr Byte } -- TODO: proper memory map!

init :: Mem
init = Mem { m = Map.empty }

read :: Mem -> Addr -> Byte
--read Mem{m} a = maybe (error (show ("read",a))) id $ Map.lookup a m
read Mem{m} a = maybe 0 id $ Map.lookup a m

write :: Mem -> Addr -> Byte -> Mem
write = undefined
