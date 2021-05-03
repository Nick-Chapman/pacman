module System(
  System(..), Eff(..), index, E(..), eNot, Reg, RomId, RomSpec(..),
  Conf(..),elaborate,
  ) where

import Control.Monad (ap,liftM)
import Code
import Value
import Rom (Rom)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

-- top level effect; declare regs, load rom etc
data System
  = FrameEffect (Eff ())
  | DeclareRom RomSpec (RomId -> System)
  | DeclareReg1 (Reg Bit -> System)
  | DeclareReg Size (Reg [Bit] -> System)

-- the core effect type
data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  CaseBit :: E Bit -> Eff Bit
  SetPixel :: XY (E Nat) -> RGB (E Nat) -> Eff ()
  GetReg :: Reg a -> Eff (E a)
  SetReg :: Show a => Reg a -> E a -> Eff ()
  And :: E Bit -> E Bit -> Eff (E Bit)
  Plus :: E Nat -> E Nat -> Eff (E Nat)
  ReadRomByte :: RomId -> E Nat -> Eff (E Nat)
  Split :: E [Bit] -> Eff [E Bit]
  Combine :: [E Bit] -> Eff (E [Bit])

  ReadMem :: E Nat -> Eff (E Nat)

index :: E [Bit] -> Int -> Eff (E Bit)
index e i = do
  bits <- Split e
  pure $ indexBits bits i

data ES = ES -- elaboration state
  { regId :: RegId
  , regs :: [(RegId,Size)]
  , romId :: RomId
  , romSpecs :: [(RomId,RomSpec)]
  }

es0 :: ES
es0 = ES { regId = 0, regs = [], romId = 101, romSpecs = [] }

data Conf = Conf { specializeRoms :: Bool }

elaborate :: Conf -> System -> IO Code
elaborate Conf{specializeRoms} = loop es0
  where
    loop :: ES -> System -> IO Code
    loop es@ES{regId,regs,romId,romSpecs} = \case
      DeclareRom spec f -> do
        loop es { romId = romId + 1, romSpecs = (romId,spec) : romSpecs } (f romId)
      DeclareReg size f -> do
        let reg = Reg size regId
        loop es { regId = regId + 1, regs = (regId,size) : regs } (f reg)
      DeclareReg1 f -> do
        let size = Size { size = 1 }
        let reg = Reg1 regId
        loop es { regId = regId + 1, regs = (regId,size) : regs } (f reg)
      FrameEffect eff -> do
        roms <- if specializeRoms then loadRoms romSpecs else pure Map.empty
        let prog = compile0 roms eff
        pure $ Code { romSpecs, regDecs = regs, entry = prog }

data CS = CS
  { u :: Int
  }

type Roms = Map RomId Rom

compile0 :: Roms -> Eff () -> Prog
compile0 roms eff0 = comp CS { u = 0 } eff0 (\_ _ -> P_Halt)
  where
    comp :: CS -> Eff a -> (CS -> a -> Prog) -> Prog
    comp s eff k = case eff of

      ReadMem{} -> undefined

      Ret a -> k s a
      Bind e f -> comp s e $ \s a -> comp s (f a) k

      SetPixel xy rgb -> P_Seq (S_SetPixel xy rgb) (k s ())
      SetReg reg exp -> P_Seq (S_SetReg reg exp) (k s ())

      CaseBit bit -> do
        case bit of
          E_Lit _ B1 -> k s B1
          E_Lit _ B0 -> k s B0
          _ -> P_If bit (k s B1) (k s B0)

      GetReg reg@(Reg size _) -> do
        shareV s size (O_Reg reg) $ \s tmp ->
          k s (E_Tmp tmp)

      GetReg reg@Reg1{} -> do
        share1 s (O_Reg reg) $ \s tmp ->
          k s (E_Tmp tmp)

      Plus e1 e2 -> do
        case (trySimpPlus (e1,e2)) of
          Just e -> k s e
          Nothing -> do
            let size = max (sizeE e1) (sizeE e2)
            let oper = O_Plus e1 e2
            shareV s size oper $ \s tmp ->
              k s (E_Tmp tmp)

      And e1 e2 -> do
        case (trySimpAnd (e1,e2)) of
          Just e -> k s e
          Nothing -> do
            let oper = O_And e1 e2
            share1 s oper $ \s tmp ->
              k s (E_Tmp tmp)

      ReadRomByte rid a -> do
        case (a,Map.lookup rid roms) of
          (E_Nat a, Just rom) -> k s (E_Nat (readRom rom a))
          _ -> do
            shareV s (Size 8) (O_ReadRomByte rid a) $ \s tmp -> do
              k s (E_Tmp tmp)

      Split e -> do
        case e of
          E_Nat xs -> k s (map (E_Lit 1) xs)
          E_Lit _ xs -> k s (map (E_Lit 1) xs)
          E_Combine xs -> k s xs
          E_Tmp tmp -> do
            let Size n = sizeE e
            k s [E_TmpIndexed tmp i | i <- reverse [0..n-1]]

      Combine es -> do
        case tryLiteralizeBits es of
          Just xs -> k s (E_Nat xs)
          Nothing ->
            k s (E_Combine es)

tryLiteralizeBits :: [E Bit] -> Maybe [Bit]
tryLiteralizeBits es = do
  let bs = [ b | e <- es, E_Lit _ b <- [e] ]
  if length bs == length es then Just bs else Nothing

share1 :: CS -> Oper Bit -> (CS -> Tmp Bit -> Prog) -> Prog
share1 s@CS{u} oper k = do
  let tmpId = TmpId { u }
  let tmp = Tmp1 tmpId
  P_Seq (S_Let tmp oper) (k s { u = u + 1 } tmp)

shareV :: CS -> Size -> Oper [Bit] -> (CS -> Tmp [Bit] -> Prog) -> Prog
shareV s@CS{u} size oper k = do
  let tmpId = TmpId { u }
  let tmp = Tmp size tmpId
  P_Seq (S_Let tmp oper) (k s { u = u + 1 } tmp)

trySimpAnd :: (E Bit, E Bit) -> Maybe (E Bit)
trySimpAnd = \case
  (z@(E_Lit _ B0), _) -> Just z
  (_, z@(E_Lit _ B0)) -> Just z
  (E_Lit _ B1, x) -> Just x
  (x, E_Lit _ B1) -> Just x
  _ ->
    Nothing

trySimpPlus :: (E Nat, E Nat) -> Maybe (E Nat)
trySimpPlus = \case
  (E_Nat n1, E_Nat n2) -> Just (E_Nat (plusNat n1 n2))
  _ -> Nothing

sizeE :: E a -> Size
sizeE = \case
  E_KeyDown{} -> Size 1
  E_Nat nat -> sizeOfNat nat
  E_Lit size _ -> size
  E_Not e -> sizeE e
  E_Tmp (Tmp size _) -> size
  E_Tmp (Tmp1 _) -> Size 1
  E_TmpIndexed _ _ -> Size 1
  E_Combine xs -> Size (length xs)
