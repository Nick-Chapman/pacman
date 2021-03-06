
-- | Values manipulated by Emulation Systems and Code.

module Value (
  YN(..), XY(..), RGB(..), Key(..), Nat, Bit(..), Size(..), ScreenSpec(..),
  checkSize, isBit1, andBit, xorBit, notBit, indexBits,
  sizedNat, nat2int, plusNat, minusNat, sizeOfNat, isZeroNat, lessNat, xorNat,
  defaultScreenSpec,
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Key
  = KeyEnter
  | KeyShift
  | KeyZ
  | KeyX
  deriving (Eq,Ord,Enum,Bounded,Show)

data YN a = YN { yes :: a, no :: a }
data Bit = B0 | B1   deriving (Generic,NFData)
type Nat = [Bit] -- MSB first (like all bit lists)
data XY a = XY { x :: a, y :: a } deriving (Eq,Ord,Functor,Generic,NFData)
data RGB a = RGB { r :: a, g :: a, b :: a } deriving (Functor,Generic,NFData)
newtype Size = Size { size :: Int } deriving newtype (Eq,Ord,Num,NFData)

data ScreenSpec = ScreenSpec { sf ::Int, size :: XY Int, emuSecsPerFrame :: Double }

defaultScreenSpec :: ScreenSpec
defaultScreenSpec = ScreenSpec { sf = 2, size = XY { x= 256, y = 256 }, emuSecsPerFrame = 1.0 / 60 }

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

xorBit :: Bit -> Bit -> Bit
xorBit = \case
  B0 -> \x -> x
  B1 -> \x -> notBit x

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
  drop 1 $ sizedNat (Size (n+1)) (pow2 n + nat2int x - nat2int y) -- ???

pow2 :: Int -> Int
pow2 n = product (replicate n 2)

isZeroNat :: Nat -> Bit
isZeroNat n = if nat2int n == 0 then B1 else B0

lessNat :: Nat -> Nat -> Bit
lessNat x y = do
  if (nat2int x < nat2int y) then B1 else B0

xorNat :: Nat -> Nat -> Nat
xorNat xs ys = [ xorBit x y | (x,y) <- zip xs ys ]
