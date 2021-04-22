
module Mem (Mem,Addr,init,read,readIO,write,writeIO) where

import Prelude hiding (init,read,readIO)
import Addr (Addr(..))
import Byte (Byte(..))
import Ram (Ram)
import Rom (Rom)
import qualified Ram (init,read,write)
import qualified Rom (load,lookup)

data Mem = Mem  -- TODO: mem mapped IO
  { rom6e :: Rom
  , rom6f :: Rom
  , rom6h :: Rom
  , rom6j :: Rom
  , ram :: Ram
  }

init :: IO Mem
init = do
  rom6e <- Rom.load 4096 "roms/pacman.6e"
  rom6f <- Rom.load 4096 "roms/pacman.6f"
  rom6h <- Rom.load 4096 "roms/pacman.6h"
  rom6j <- Rom.load 4096 "roms/pacman.6j"
  let ram = Ram.init (4 * 1024 + 256)
  pure $ Mem { rom6e, rom6f, rom6h, rom6j, ram }

read :: Mem -> Addr -> Byte
read Mem{rom6e,rom6f,rom6h,rom6j,ram} a = if
  | (a < 0x1000) -> Rom.lookup rom6e (fromIntegral a)
  | (a < 0x2000) -> Rom.lookup rom6f (fromIntegral a - 0x1000)
  | (a < 0x3000) -> Rom.lookup rom6h (fromIntegral a - 0x2000)
  | (a < 0x4000) -> Rom.lookup rom6j (fromIntegral a - 0x3000)
  | (a >= startRam && a < endRam) -> do
      undefined ram Ram.read
  | otherwise ->
    error (show ("Mem.read",a))

write :: Mem -> Addr -> Byte -> Mem
write mem@Mem{ram} a b = if
  | (a >= startRam && a < endRam) -> do
      let! ram' = Ram.write ram (fromIntegral (a - startRam)) b
      mem { ram = ram' }
  | otherwise ->
    error (show ("Mem.write",a))

startRam,endRam :: Addr
startRam = 0x4000
endRam = 0x5100

writeIO :: Mem -> Addr -> Byte -> IO Mem
writeIO m a b = if
  | a == 0xFFFE -> do
      errLikeZazu a b
      pure m
  | a == 0xFFFD -> do
      errLikeZazu a b
      pure m
  | otherwise -> do
      let! m' = write m a b
      pure m'

errLikeZazu :: Addr -> Byte -> IO ()
errLikeZazu a b = putStrLn $ "ERR: write " ++ show b ++ " at " ++ show a


readIO :: Mem -> Addr -> IO Byte
readIO m a = do
  --print ("Mem.read",a)
  let! b = read m a
  pure b
