module System(
  System(..), Eff(..), Reg, RomId, RomSpec(..), RamId,
  Conf(..),elaborate,
  -- effect-free combinators
  E, sizeE, eLit, eSized, eNot, keyDown, combine, split, index,
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
  = FrameEffect ScreenSpec (Eff ())
  | DeclareRom RomSpec (RomId -> System)
  | DeclareReg1 String (Reg Bit -> System)
  | DeclareReg1i String Bit (Reg Bit -> System)
  | DeclareReg String Size (Reg [Bit] -> System)
  | DeclareRam Size (RamId -> System)

-- the core effect type
data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Repeat :: Int -> Eff () -> Eff ()
  --CaseBit :: E Bit -> Eff Bit
  Trace :: String -> [E Nat] -> Eff ()
  SetPixel :: XY (E Nat) -> RGB (E Nat) -> Eff ()
  GetReg :: Reg a -> Eff (E a)
  SetReg :: Show a => Reg a -> E a -> Eff ()
  And :: E Bit -> E Bit -> Eff (E Bit)
  Plus :: E Nat -> E Nat -> Eff (E Nat)
  Minus :: E Nat -> E Nat -> Eff (E Nat)
  IsZero :: E Nat -> Eff (E Bit)
  Mux :: E Bit -> YN (E [Bit])-> Eff (E [Bit])
  ReadRomByte :: RomId -> E Nat -> Eff (E Nat)
  ReadRam :: RamId -> E Nat -> Eff (E Nat)
  WriteRam :: RamId -> E Nat -> E Nat -> Eff ()
  Ite :: E Bit -> (Bit -> Eff ()) -> Eff ()

data ES = ES -- elaboration state
  { regId :: RegId
  , ramId :: RamId
  , regs :: [RegDec]
  , romId :: RomId
  , romSpecs :: [(RomId,RomSpec)]
  , ramDecs :: [(RamId,Size)]
  }

es0 :: ES
es0 = ES { regId = 0, ramId = 1, regs = [], romId = 101, romSpecs = [], ramDecs = [] }

data Conf = Conf { specializeRoms :: Bool }

elaborate :: Conf -> System -> IO Code
elaborate Conf{specializeRoms} = loop es0
  where
    loop :: ES -> System -> IO Code
    loop es@ES{regId,ramId,regs,romId,romSpecs,ramDecs} = \case
      DeclareRom spec f -> do
        loop es { romId = romId + 1, romSpecs = (romId,spec) : romSpecs } (f romId)

      DeclareReg name size f -> do
        let reg = Reg size regId
        let init = zeroOf size
        let regDec = RegDec { rid = regId, size, init, name }
        loop es { regId = regId + 1, regs = regDec : regs } (f reg)

      DeclareReg1 name f -> do
        let size = Size { size = 1 }
        let reg = Reg1 regId
        let init = zeroOf size
        let regDec = RegDec { rid = regId, size, init, name }
        loop es { regId = regId + 1, regs = regDec : regs } (f reg)

      DeclareReg1i name bit f -> do
        let size = Size { size = 1 }
        let reg = Reg1 regId
        let init = [bit]
        let regDec = RegDec { rid = regId, size, init, name }
        loop es { regId = regId + 1, regs = regDec : regs } (f reg)

      DeclareRam size f -> do
        loop es { ramId = ramId + 1, ramDecs = (ramId,size) : ramDecs } (f ramId)
      FrameEffect screenSpec eff -> do
        roms <- if specializeRoms then loadRoms romSpecs else pure Map.empty
        let prog = compile0 roms eff
        pure $ Code { romSpecs, ramDecs, regDecs = regs, screenSpec, entry = prog }


zeroOf :: Size -> [Bit]
zeroOf (Size n) = take n (repeat B0)

data CS = CS
  { u :: Int
  , regs1 :: Map RegId (E Bit) -- 1-bit regs
  , regsV :: Map RegId (E [Bit]) -- vector regs
  }

{-
iteCS :: E Bit -> CS -> CS -> CS
iteCS = undefined

ite :: E Bit -> E a -> E a
ite sel yes no = if yes == no then yes else ...
-}

type Roms = Map RomId Rom

type Res = (CS,Prog)

doStep :: Step -> Res -> Res
doStep step = doProg (P_Step step)

doProg :: Prog -> Res -> Res
doProg prog1 (cs,prog2) = (cs, P_Seq prog1 prog2)

sequenceSteps :: [Step] -> Prog
sequenceSteps = \case
  [] -> P_Halt
  x1:xs -> P_Seq (P_Step x1) (sequenceSteps xs)

flushRegs :: CS -> () -> Res
flushRegs = \cs@CS{regs1,regsV} () -> do
  let steps1 = [ S_SetReg (Reg1 rid) e | (rid,e) <- Map.toList regs1 ]
  let stepsV = [ S_SetReg (Reg (sizeE e) rid) e | (rid,e) <- Map.toList regsV ]
  (cs { regs1 = Map.empty, regsV = Map.empty }
    , sequenceSteps (steps1 ++ stepsV))

compile0 :: Roms -> Eff () -> Prog
compile0 roms eff0 = do
  let s0 = CS { u = 0, regs1 = Map.empty, regsV = Map.empty }
  let (_,prog) :: Res = comp s0 eff0 flushRegs
  prog
  where
    comp :: CS -> Eff a -> (CS -> a -> Res) -> Res
    comp s@CS{regs1,regsV} eff k = case eff of

      Ret a -> k s a
      Bind e f -> comp s e $ \s a -> comp s (f a) k

      Repeat n e -> do
        let (s1,progF) = flushRegs s ()
        let (s2,progE) = comp s1 e flushRegs
        doProg progF (doProg (P_Repeat n progE) (k s2 ()))

      Ite bit f -> do
        case bit of
          E_Lit _ B1 -> comp s (f B1) k
          E_Lit _ B0 -> comp s (f B0) k
          _ -> do
            -- TODO: we can do better than this;
            -- only flush regs assigned differently in the two branches
            let (s0,p0) = flushRegs s ()
            let (s1,p1) = comp s0 (f B1) flushRegs
            let (s2,p2) = comp s1 (f B0) flushRegs
            doProg p0 (doProg (P_If bit p1 p2) (k s2 ()))

            -- Try to merge current-reg maps...
            {-let s0 = s
            let (s1@CS{u=u1},p1) = comp s0 (f B1) $ \s () -> (s,P_Halt)
            let (s2@CS{u=u2},p2) = comp s0 {u=u1} (f B0) $ \s () -> (s,P_Halt)
            let s3 :: CS = (iteCS bit s1 s2) {u = u2}
            doProg p1 (doProg p2 (k s3 ()))-}

      SetPixel xy rgb -> doStep (S_SetPixel xy rgb) (k s ())
      Trace tag exps -> doStep (S_Trace tag exps) (k s ())

      {-CaseBit bit -> do -- DONT USE ME, I BLOW UP
        case bit of
          E_Lit _ B1 -> k s B1
          E_Lit _ B0 -> k s B0
          _ -> do
            let (s1,p1) = k s B1
            let (s2,p2) = k s1 B1
            (s2, P_If bit p1 p2)-}

      SetReg (Reg1 rid) exp ->
        k s { regs1 = Map.insert rid exp regs1 } ()

      SetReg (Reg _ rid) exp -> do
        k s { regsV = Map.insert rid exp regsV } ()

      GetReg reg@(Reg1 rid) -> do
        case Map.lookup rid regs1 of
          Just e -> k s e
          Nothing -> do
            share1 s (O_Reg reg) $ \s tmp ->
              k s (E_Tmp tmp)

      GetReg reg@(Reg size rid) -> do
        case Map.lookup rid regsV of
          Just e -> k s e
          Nothing -> do
            shareV s size (O_Reg reg) $ \s tmp ->
              k s (E_Tmp tmp)

      Plus e1 e2 -> do
        case (trySimpPlus (e1,e2)) of
          Just e -> k s e
          Nothing -> do
            let oper = O_Plus e1 e2
            let size = max (sizeE e1) (sizeE e2)
            shareV s size oper $ \s tmp ->
              k s (E_Tmp tmp)

      Minus e1 e2 -> do
        case (trySimpPlus (e1,e2)) of
          Just e -> k s e
          Nothing -> do
            let oper = O_Minus e1 e2
            let size = max (sizeE e1) (sizeE e2)
            shareV s size oper $ \s tmp ->
              k s (E_Tmp tmp)

      IsZero e -> do
        let oper = O_IsZero e
        share1 s oper $ \s tmp ->
          k s (E_Tmp tmp)

      Mux sel yn@YN{yes,no} -> do
        case sel of
          E_Lit _ B1 -> k s yes
          E_Lit _ B0 -> k s no
          _ -> do
            let oper = O_Mux sel yn
            let size = ensureSameSize oper (sizeE yes) (sizeE no)
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

      ReadRam id a -> do
        shareV s (Size 8) (O_ReadRam id a) $ \s tmp -> do
          k s (E_Tmp tmp)

      WriteRam id a b -> do
        doStep (S_WriteRam id a b) (k s ())

ensureSameSize :: Show a => Oper a -> Size -> Size -> Size
ensureSameSize oper z1 z2 =
  if z1 == z2 then z1 else error (show ("ensureSameSize",oper,z1,z2))

share1 :: CS -> Oper Bit -> (CS -> Tmp Bit -> Res) -> Res
share1 s@CS{u} oper k = do
  let tmpId = TmpId { u }
  let tmp = Tmp1 tmpId
  doStep (S_Let tmp oper) (k s { u = u + 1 } tmp)

shareV :: CS -> Size -> Oper [Bit] -> (CS -> Tmp [Bit] -> Res) -> Res
shareV s@CS{u} size oper k = do
  let tmpId = TmpId { u }
  let tmp = Tmp size tmpId
  doStep (S_Let tmp oper) (k s { u = u + 1 } tmp)

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

----------------------------------------------------------------------
-- operators without effects:
--   easier to compose for caller; make it clear that no Ops are generated

keyDown :: Key -> E Bit
keyDown = E_KeyDown

----------------------------------------------------------------------
-- split/combine

-- TODO: rename: combine/split --> bundle/unbundle (like  "clash")

combine :: [E Bit] -> E [Bit]
combine es =
  case tryLiteralizeBits es of
    Just xs -> E_Nat xs
    Nothing -> E_Combine es

tryLiteralizeBits :: [E Bit] -> Maybe [Bit]
tryLiteralizeBits es = do
  let bs = [ b | e <- es, E_Lit _ b <- [e] ]
  if length bs == length es then Just bs else Nothing

split :: E [Bit] -> [E Bit]
split = \case
  E_Nat xs -> map (E_Lit 1) xs
  E_Lit _ xs -> map (E_Lit 1) xs
  E_Combine xs -> xs
  e@(E_Tmp tmp) -> do
    let Size n = sizeE e
    [E_TmpIndexed tmp i | i <- reverse [0..n-1]]

index :: E [Bit] -> Int -> E Bit
index e i = reverse (split e) !! i

----------------------------------------------------------------------
-- literals

eLit :: Size -> a -> E a
eLit = E_Lit

eSized :: Size -> Int -> E Nat
eSized size i = do E_Nat (sizedNat size i)
