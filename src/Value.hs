module Value (
  XY(..), RGB(..), Key(..), Nat, Bit(..),
  Size(..),
  zeroOf,checkSize,fromBits,andBit,plusBits,notBit,indexBits,bitsOfInt,
  ) where

data Key
  = KeyEnter
  | KeyShift
  | KeyZ
  | KeyX
  deriving (Eq,Ord,Enum,Bounded,Show)

data XY a = XY { x :: a, y :: a } deriving (Eq,Ord,Functor)
data RGB a = RGB { r :: a, g :: a, b :: a } deriving (Functor)

type Nat = [Bit]

data Bit = B0 | B1

newtype Size = Size { size :: Int } deriving (Eq,Ord,Num)

instance Show Size where show Size{size} = "#" ++ show size

checkSize :: Size -> [Bit] -> [Bit]
checkSize Size{size} xs =
  if length xs == size then xs else
    error (show ("checkSize",size,xs))

-- TODO: use Integer when converting to/from [Bit]

bitsOfInt :: Size -> Int -> [Bit] -- lsb..msb
bitsOfInt Size{size} n =
  if n < 0 then error (show ("bitsOfInt<0",n)) else do
    let xs = loop n
    if length xs > size then error (show ("bitsOfInt-too-small",size,xs)) else
      xs ++ take (size - length xs) (repeat B0)
  where
    loop n = if
      | n == 0 -> []
      | n `mod` 2 == 1 -> B1 : loop (n `div` 2)
      | otherwise -> B0 : loop (n `div` 2)

fromBits :: [Bit] -> Int
fromBits = \case
  [] -> 0
  B0:xs -> 2 * fromBits xs
  B1:xs -> 2 * fromBits xs + 1

indexBits :: Show a => [a] -> Int -> a
indexBits xs i =
  if i < 0 then error "indexBits:i<0" else
    if i >= length xs then error (show ("indexBits:too-large",i,xs)) else
      xs !! i

notBit :: Bit -> Bit
notBit = \case B1 -> B0; B0 -> B1

andBit :: Bit -> Bit -> Bit
andBit = \case
  B0 -> \_ -> B0
  B1 -> \x -> x

plusBits :: [Bit] -> [Bit] -> [Bit]
plusBits x y = do
  let nx = length x
  let ny = length y
  let size = max nx ny
  take size (bitsOfInt (Size (size+1)) (fromBits x + fromBits y))

zeroOf :: Int -> [Bit]
zeroOf size = take size (repeat B0)

instance Show Bit where show = \case B0 -> "0"; B1 -> "1"
instance Show a => Show (XY a) where show XY{x,y} = show (x,y)
instance Show a => Show (RGB a) where show RGB{r,g,b} = "RGB" ++ show (r,g,b)
