
module Rom (Rom,load,lookup) where

import Control.Monad (unless)
import Data.Array (Array,(!),listArray)
import Data.Word8 (Word8)
import Prelude hiding (lookup)
import qualified Data.ByteString as BS (readFile,unpack)

data Rom = Rom { path :: FilePath, size :: Int, bytesA :: Array Int Word8 }

load :: Int -> FilePath -> IO Rom
load expected path = do
  bytes <- loadBytes path
  let size = length bytes
  unless (size == expected) $
    error $ "Rom.load: #bytes=" <> show size <> " (expected:" <> show expected <> ")"
  let bytesA = listArray (0,size-1) bytes
  pure $ Rom { path, size, bytesA }

loadBytes :: FilePath -> IO [Word8]
loadBytes path = BS.unpack <$> BS.readFile path

lookup :: Rom -> Int -> Word8
lookup Rom{path,size,bytesA} i = if
  | i < size -> bytesA ! i
  | otherwise -> error (show ("Rom.lookup",path,i,size))
