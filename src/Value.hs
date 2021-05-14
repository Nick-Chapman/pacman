module Value (
  XY(..), RGB(..), Key(..), Nat, Bit(..),
  Size(..),
  checkSize, isBit1, andBit, notBit, indexBits,
  sizedNat, nat2int, plusNat, minusNat, sizeOfNat, isZeroNat,
  ) where

data Key
  = KeyEnter
  | KeyShift
  | KeyZ
  | KeyX
  deriving (Eq,Ord,Enum,Bounded,Show)

data Bit = B0 | B1
type Nat = [Bit] -- MSB first (like all bit lists)
data XY a = XY { x :: a, y :: a } deriving (Eq,Ord,Functor)
data RGB a = RGB { r :: a, g :: a, b :: a } deriving (Functor)
newtype Size = Size { size :: Int } deriving (Eq,Ord,Num)

instance Show Bit where show = \case B0 -> "0"; B1 -> "1"
instance Show a => Show (XY a) where show XY{x,y} = show (x,y)
instance Show a => Show (RGB a) where show RGB{r,g,b} = "RGB" ++ show (r,g,b)
instance Show Size where show Size{size} = "#" ++ show size

isBit1 :: Bit -> Bool
isBit1 = \case B1 -> True; B0 -> False

notBit :: Bit -> Bit
notBit = \case B1 -> B0; B0 -> B1

andBit :: Bit -> Bit -> Bit
andBit = \case
  B0 -> \_ -> B0
  B1 -> \x -> x

checkSize :: Size -> [Bit] -> [Bit]
checkSize Size{size} xs =
  if length xs == size then xs else
    error (show ("checkSize",size,xs))

indexBits :: Show a => [a] -> Int -> a -- index from LSB
indexBits xs i =
  if i < 0 then error "indexBits:i<0" else
    if i >= length xs then error (show ("indexBits:too-large",i,xs)) else
      reverse xs !! i

sizeOfNat :: Nat -> Size
sizeOfNat xs = Size (length xs)

sizedNat :: Size -> Int -> Nat
sizedNat Size{size} i =
  if i < 0 then error (show ("sizedNat<0",i)) else do
    let xs = loop i
    if length xs > size then error (show ("sizedNat:too-small",size,xs)) else
      take (size - length xs) (repeat B0) ++ (reverse xs)
  where
    loop n = if
      | n == 0 -> []
      | n `mod` 2 == 1 -> B1 : loop (n `div` 2)
      | otherwise -> B0 : loop (n `div` 2)

nat2int :: Nat -> Int
nat2int xs = fromBits (reverse xs)
  where
    fromBits :: [Bit] -> Int
    fromBits = \case
      [] -> 0
      B0:xs -> 2 * fromBits xs
      B1:xs -> 2 * fromBits xs + 1

plusNat :: Nat -> Nat -> Nat
plusNat x y = do
  let nx = length x
  let ny = length y
  let n = max nx ny
  drop 1 $ sizedNat (Size (n+1)) (nat2int x + nat2int y)

minusNat :: Nat -> Nat -> Nat
minusNat x y = do
  let nx = length x
  let ny = length y
  let n = max nx ny
  drop 1 $ sizedNat (Size (n+1)) (nat2int x - nat2int y) -- ???

isZeroNat :: Nat -> Bit
isZeroNat n = if nat2int n == 0 then B1 else B0
