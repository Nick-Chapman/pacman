module System(
  System(..), Eff(..), index, E(..), eNot, RomId, RomSpec(..),
  elaborate,
  ) where

import Control.Monad (ap,liftM)
import Code
import Value

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
  LitV :: [a] -> Eff (E [a])
  Split :: Eff (E [Bit]) -> Eff [E Bit]
  Combine :: [E Bit] -> Eff (E [Bit])

index :: Eff (E [Bit]) -> Int -> Eff (E Bit)
index eff i = do
  bits <- Split eff
  pure $ indexBits bits i


elaborate :: System -> Code
elaborate = loop ES { regId = 0, regs = [], romId = 101, roms = [] }
  where
    loop :: ES -> System -> Code
    loop es@ES{regId,regs,romId,roms} = \case
      DeclareRom spec f -> do
        loop es { romId = romId + 1, roms = (romId,spec) : roms } (f romId)
      DeclareReg size f -> do
        let reg = Reg size regId
        loop es { regId = regId + 1, regs = (regId,size) : regs } (f reg)
      DeclareReg1 f -> do
        let size = Size { size = 1 }
        let reg = Reg1 regId
        loop es { regId = regId + 1, regs = (regId,size) : regs } (f reg)
      FrameEffect eff -> do
        let prog = compile0 eff
        Code { romSpecs = roms, regDecs = regs, entry = prog }

data ES = ES -- elaboration state
  { regId :: RegId
  , regs :: [(RegId,Size)]
  , romId :: RomId
  , roms :: [(RomId,RomSpec)]
  }

data CS = CS { u :: Int }

compile0 :: Eff () -> Prog
compile0 eff0 = comp CS { u = 0 } eff0 (\_ _ -> P_Halt)
  where
    comp :: CS -> Eff a -> (CS -> a -> Prog) -> Prog
    comp s eff k = case eff of
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
        shareO s size (O_Reg reg) $ \s tmp ->
          k s (E_Tmp tmp)

      GetReg reg@Reg1{} -> do
        share1 s (O_Reg reg) $ \s tmp ->
          k s (E_Tmp tmp)

      Plus e1 e2 -> do
        let size = max (sizeE e1) (sizeE e2)
        let oper = O_Plus e1 e2
        shareO s size oper $ \s tmp ->
          k s (E_Tmp tmp)

      And e1 e2 -> do
        case (trySimpAnd (e1,e2)) of
          Just e -> k s e
          Nothing -> do
            let oper = O_And e1 e2
            share1 s oper $ \s tmp ->
              k s (E_Tmp tmp)

      ReadRomByte rid a -> do
        shareO s (Size 8) (O_ReadRomByte rid a) $ \s tmp -> do
          k s (E_Tmp tmp)

      Split (LitV xs) -> do
        k s (map (E_Lit 1) xs)

      Split eff -> do
        comp s eff $ \s e -> do
          let size@(Size n) = sizeE e
          shareO s size (O_Exp e) $ \s tmp -> do
            k s [E_TmpIndexed tmp i | i <- [0..n-1]]

      LitV xs -> do
        k s (E_Lit (Size (length xs)) xs)

      Combine e -> do
        k s (E_Combine e)

share1 :: CS -> Oper Bit -> (CS -> Tmp Bit -> Prog) -> Prog
share1 s@CS{u} oper k = do
  case oper of
    --O_Exp (E_Tmp tmp) -> k s tmp
    _ -> do
      let tmpId = TmpId { u }
      let tmp = Tmp1 tmpId
      P_Seq (S_Let tmp oper) $
        k s { u = u + 1 } tmp

shareO :: CS -> Size -> Oper [Bit] -> (CS -> Tmp [Bit] -> Prog) -> Prog
shareO s@CS{u} size oper k = do
  case oper of
    O_Exp (E_Tmp tmp) -> k s tmp
    _ -> do
      let tmpId = TmpId { u }
      let tmp = Tmp size tmpId
      P_Seq (S_Let tmp oper) $
        k s { u = u + 1 } tmp

trySimpAnd :: (E Bit, E Bit) -> Maybe (E Bit)
trySimpAnd = \case
  (z@(E_Lit _ B0), _) -> Just z
  (_, z@(E_Lit _ B0)) -> Just z
  (E_Lit _ B1, x) -> Just x
  (x, E_Lit _ B1) -> Just x
  _ ->
    Nothing

sizeE :: E a -> Size
sizeE = \case
  E_KeyDown{} -> Size 1
  E_Lit size _ -> size
  E_Not e -> sizeE e
  E_Tmp (Tmp size _) -> size
  E_Tmp (Tmp1 _) -> Size 1
  E_TmpIndexed _ _ -> Size 1
  E_Combine es -> sum [ sizeE e | e <- es ]
