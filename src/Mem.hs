
module Mem (Mem,Addr,init,read,readIO,write,writeIO) where

import Prelude hiding (init,read,readIO)
import Addr (Addr(..))
import Byte (Byte(..))
import Ram (Ram)
import Rom (Rom)
import qualified Ram (init,read,write)
import qualified Rom (load,lookup)

data Mem = Mem { rom :: Rom , ram :: Ram } -- TODO: mem mapped IO

init :: IO Mem
init = do
  rom <- Rom.load 4096 "roms/pacman.6e" -- TODO: 3 other roms! merge!!
  let ram = Ram.init (4 * 1024)
  pure $ Mem { rom, ram }

read :: Mem -> Addr -> Byte
read Mem{rom,ram} a = if
  | (a < startRam) -> Rom.lookup rom (fromIntegral a)
  | (a >= startRam && a < endRam) -> do
      undefined ram Ram.read
  | otherwise ->
    error (show ("Mem.read",a))

write :: Mem -> Addr -> Byte -> Mem
write mem@Mem{ram} a b = if
--  | a == 0xFFFF -> mem -- TODO: ignored
--  | a == 0xFFFE -> mem -- TODO: ignored
  | (a >= startRam && a < endRam) -> do
      let! ram' = Ram.write ram (fromIntegral (a - startRam)) b
      mem { ram = ram' }
  | otherwise ->
    error (show ("Mem.write",a))

startRam,endRam :: Addr
startRam = 0x4000
endRam = 0x5000

writeIO :: Mem -> Addr -> Byte -> IO Mem
writeIO m a b = do
  --print ("Mem.write",a,b)
  let! mem' = write m a b
  pure mem'

readIO :: Mem -> Addr -> IO Byte
readIO m a = do
  --print ("Mem.read",a)
  let! b = read m a
  pure b
