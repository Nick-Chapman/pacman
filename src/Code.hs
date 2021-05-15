module Code (
  Code(..), Prog(..), Step(..), E(..), Oper(..), eNot,
  RegId, Reg(..), Tmp(..), TmpId(..), RegDec(..), RomId, RomSpec(..), RamId,
  pretty,
  loadRoms, readRom,
  initialize, Context, State, runForOneFrame, Keys(..), Picture(..),
  ) where

import Control.DeepSeq (NFData)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import Prelude hiding (init)
import Ram (Ram)
import Rom (Rom)
import Text.Printf (printf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Ram (init,read,write)
import qualified Rom (load,lookup)

import Value

-- full generated code. includes decs & prog
data Code = Code
  { regDecs :: [RegDec]
  , ramDecs :: [(RamId,Size)]
  , romSpecs :: [(RomId,RomSpec)]
  , screenSpec :: ScreenSpec
  , entry :: Prog
  }

data RegDec = RegDec
  { rid :: RegId
  , size :: Size
  , init :: [Bit]
  , name :: String -- user name
  }

-- statement in the generated program, works in context of some decs
data Prog where
  P_Halt :: Prog
  P_Seq :: Prog -> Prog -> Prog
  P_Step :: Step -> Prog
  P_If :: E Bit -> Prog -> Prog -> Prog
  P_Repeat :: Int -> Prog -> Prog

-- basic program step (statement), which is sequenced in a program
data Step where
  S_Let :: Show a => Tmp a -> Oper a -> Step
  S_WriteRam :: RamId -> E Nat -> E Nat -> Step
  S_SetReg :: Show a => Reg a -> E a -> Step
  S_SetPixel :: XY (E Nat) -> RGB (E Nat) -> Step
  S_Trace :: String -> [E Nat] -> Step

-- operation (non atomic/pure expression), will always be let-bound
-- these things MUST be name
data Oper a where
  O_Reg :: Reg a -> Oper a
  O_And :: E Bit -> E Bit -> Oper Bit
  O_Plus :: E Nat -> E Nat -> Oper Nat
  O_Minus :: E Nat -> E Nat -> Oper Nat
  O_IsZero :: E Nat -> Oper Bit
  O_Mux :: E Bit -> YN (E a) -> Oper a
  O_ReadRomByte :: RomId -> E Nat -> Oper Nat
  O_ReadRam :: RamId -> E Nat -> Oper Nat

-- program expressions; atomic/pure, so can be freely shared
-- knows it's size
-- these things must *not* be named
data E a where
  E_KeyDown :: Key -> E Bit
  E_Nat :: Nat -> E Nat
  E_Lit :: Size -> a -> E a
  E_Not :: E Bit -> E Bit
  E_Tmp :: Tmp a -> E a
  E_TmpIndexed :: Tmp [Bit] -> Int -> E Bit
  E_Combine :: [E Bit] -> E [Bit]

eNot :: E Bit -> E Bit
eNot = \case
  E_Lit z B1 -> E_Lit z B0
  E_Lit z B0 -> E_Lit z B1
  E_Not ebar -> ebar
  e -> E_Not e

data Reg a where
  Reg :: Size -> RegId -> Reg [Bit]
  Reg1 :: RegId -> Reg Bit

data Tmp a where
  Tmp :: Size -> TmpId -> Tmp [Bit]
  Tmp1 :: TmpId -> Tmp Bit

data RomSpec = RomSpec { path :: String, size :: Int }

newtype RegId = RegId { u :: Int } deriving newtype (Eq,Ord,Num,NFData)
newtype TmpId = TmpId { u :: Int } deriving (Eq,Ord)
newtype RomId = RomId { u :: Int } deriving newtype (Eq,Ord,Num)
newtype RamId = RamId { u :: Int } deriving newtype (Eq,Ord,Num,NFData)

data Context = Context
  { roms :: Map RomId Rom
  }

-- everything which can form a saved-state
data State = State
  { regs :: Map RegId [Bit]
  , rams :: Map RamId Ram
  }
  deriving (Generic,NFData)

newtype Keys = Keys { pressed :: Set Key }

data Picture where
  Draw :: XY Int -> RGB Int -> Picture
  Pictures :: [Picture] -> Picture
  deriving (Generic,NFData)

initialize :: Code -> IO (ScreenSpec,Context,State,Prog) -- IO to load roms from file
initialize Code{entry=prog,regDecs,romSpecs,ramDecs,screenSpec} = do
  let regs = Map.fromList [ (rid,init) | RegDec{rid,init} <- regDecs ]
  let rams = Map.fromList [ (id,ram) | (id,Size n) <- ramDecs, let ram = Ram.init n ]
  roms <- loadRoms romSpecs
  pure $ (screenSpec,Context {roms},State {regs,rams},prog)


loadRoms :: [(RomId,RomSpec)] -> IO (Map RomId Rom)
loadRoms romSpecs =
  Map.fromList <$> sequence
  [ do rom <- Rom.load size path; pure $ (id,rom)
  | (id,RomSpec {path,size}) <- romSpecs
  ]


-- make no use of IO, but nice for debug
runForOneFrame :: Prog -> Context -> State -> Keys -> IO (Picture,State)
runForOneFrame prog context state0 keys = do
  let rs = RS { context, state = state0, keys, screen = screen0, tmps = Map.empty }
  runProg rs prog >>= \case
    RS{screen,state} ->
      pure (pictureScreen screen, state)

data RS = RS
  { context :: Context
  , keys :: Keys
  , state :: State
  , screen :: Screen
  , tmps :: Tmps
  }

type Tmps = Map TmpId [Bit]

runProg :: RS -> Prog -> IO RS
runProg rs = \case
  P_Halt -> pure rs
  P_Seq p1 p2 -> do rs <- runProg rs p1; runProg rs p2
  P_Step step -> evalStep rs step
  P_If cond this that -> do
    case evalE rs cond of
      B1 -> runProg rs this
      B0 -> runProg rs that
  P_Repeat n p -> loop 0 rs
    where
      loop i rs =
        if i < n then do rs <- runProg rs p; loop (i+1) rs
        else pure rs

evalStep :: RS -> Step -> IO RS
evalStep rs@RS{screen,state,tmps} = \case
  S_Let tmp oper -> do
    pure rs { tmps = bindTmp tmps (evalOper rs oper) tmp }
  S_WriteRam id a b ->
    pure rs { state = updateRam state id (evalE rs a) (evalE rs b) }
  S_SetReg reg e ->
    pure rs { state = updateReg state (evalE rs e) reg }
  S_SetPixel xy rgb -> do
    pure rs { screen =
         setScreenPixel
         screen
         (fmap (nat2int . evalE rs) xy)
         (fmap (nat2int . evalE rs) rgb)
       }
  S_Trace tag exps -> do
    let vs = map (nat2int . evalE rs) exps
    let hex :: Int -> String = printf "%X"
    putStrLn $ tag ++ ": " ++ show (map hex vs)
    pure rs

bindTmp :: Tmps -> a -> Tmp a -> Tmps
bindTmp tmps val = \case
  Tmp1 id ->
    update tmps id [val]
  Tmp size id -> do
    update tmps id (checkSize size val)

updateReg :: State -> a -> Reg a -> State
updateReg s@State{regs} val = \case
  Reg1 id ->
    s { regs = update regs id [val] }
  Reg size id -> do
    s { regs = update regs id (checkSize size val) }

evalOper :: RS -> Oper a -> a
evalOper rs@RS{context=Context{roms},state=State{regs,rams}} = \case
  O_And e1 e2 -> andBit (evalE rs e1) (evalE rs e2)
  O_Plus e1 e2 -> plusNat (evalE rs e1) (evalE rs e2)
  O_Minus e1 e2 -> minusNat (evalE rs e1) (evalE rs e2)
  O_IsZero e -> isZeroNat (evalE rs e)
  O_Mux sel YN{yes,no} ->
    evalE rs (if isBit1 (evalE rs sel) then yes else no)
  O_Reg (Reg size id) -> do
    checkSize size (look regs id)
  O_Reg (Reg1 id) ->
    case (look regs id) of
      [b] -> b
      bits -> error (show ("evalE/Reg1",id,length bits))
  O_ReadRomByte romId a ->
    readRom (look roms romId) (evalE rs a)
  O_ReadRam ramId a ->
    readRam (look rams ramId) (evalE rs a)


updateRam :: State -> RamId -> Nat -> Nat -> State
updateRam state@State{rams} id a b = do
  let ram = look rams id
  let ram' = Ram.write ram (nat2int a) (fromIntegral (nat2int b))
  state { rams = update rams id ram' }

readRam :: Ram -> Nat -> Nat
readRam ram a =
  sizedNat (Size 8) (fromIntegral (Ram.read ram (nat2int a)))

readRom :: Rom -> Nat -> Nat
readRom rom a = do
  sizedNat (Size 8) (fromIntegral (Rom.lookup rom (nat2int a)))
  --bitsOfInt (fromIntegral (Rom.lookup rom (fromBits a)))

evalE :: RS -> E a -> a
evalE rs@RS{keys=Keys{pressed}} = \case
  E_KeyDown key -> if Set.member key pressed then B1 else B0
  E_Nat a -> a
  E_Lit _ a -> a
  E_Not e -> notBit (evalE rs e)
  E_Tmp tmp -> evalTmp rs tmp
  E_TmpIndexed e i -> indexBits (evalTmp rs e) i
  E_Combine es -> [evalE rs e | e <- es]

evalTmp :: RS -> Tmp a -> a
evalTmp RS{tmps} = \case
  Tmp size id ->
    checkSize size (look tmps id)
  Tmp1 id ->
    case (look tmps id) of
      [b] -> b
      bits -> error (show ("evalE/Tmp1",id,length bits))

look :: (Ord k, Show k) => Map k v -> k -> v
look m k = maybe (error (show ("look/missing",k))) id $ Map.lookup k m

update :: (Ord k, Show k) => Map k v -> k -> v -> Map k v
update m k v = Map.insert k v m

----------------------------------------------------------------------
-- Screen (canvas to collect the pixels)

data Screen = Screen { m :: Map (XY Int) (RGB Int)}

screen0 :: Screen
screen0 = Screen { m = Map.empty }

setScreenPixel :: Screen -> XY Int -> RGB Int -> Screen
setScreenPixel Screen{m} xy rgb = Screen { m = Map.insert xy rgb m }

pictureScreen :: Screen -> Picture
pictureScreen Screen{m} = Pictures [ Draw xy rgb | (xy,rgb) <- Map.toList m ]

----------------------------------------------------------------------
-- pretty/show

pretty :: Pretty a => a -> String
pretty = unlines . lay

class Pretty a where
  lay :: a -> [String]

instance Pretty Code where
  lay Code{romSpecs,regDecs,ramDecs,entry} =
    [ "rom " ++ show id ++ " : " ++ show spec | (id,spec) <- romSpecs ] ++
    [ "reg " ++ show rid ++ " : " ++ show size ++ " // " ++ name
    | RegDec {rid,size,name} <- regDecs ] ++
    [ "ram " ++ show id ++ " : " ++ show size | (id,size) <- ramDecs ] ++
    lay entry

instance Pretty Prog where
  lay = \case
    P_Halt -> []
    P_Seq p1 p2 -> lay p1 ++ lay p2
    P_Step s -> lay s
    P_If cond this that ->
      ["if (" ++ show cond ++ ") {"]
      ++ tab (lay this)
      ++ ["} else {"]
      ++ tab (lay that) ++ ["}"]
    P_Repeat n prog ->
      ["repeat (" ++ show n ++ ") {"]
      ++ tab (lay prog)
      ++ ["}"]

instance Pretty Step where lay x = [show x]

tab :: [String] -> [String]
tab xs = [ "  " ++ x | x <- xs ]

instance Show Code where show = pretty

instance Show Step where
  show = \case
    S_Let tmp oper ->
      "let " ++ show tmp ++ " : " ++ show (sizeOfTmp tmp) ++ " = " ++ show oper
    S_WriteRam id a b -> show id ++ "[" ++ show a ++ "] := " ++ show b
    S_SetReg reg exp -> show reg ++ " := " ++ show exp
    S_SetPixel xy rgb -> "set-pixel " ++ show xy ++ " := " ++ show rgb
    S_Trace tag exps -> "trace: " ++ tag ++ " = " ++ show exps

sizeOfTmp :: Tmp a -> Size
sizeOfTmp = \case
  Tmp1 _ -> Size 1
  Tmp size _ -> size

instance Show a => Show (Oper a) where
  show = \case
    O_And e1 e2 -> show e1 ++ " & " ++ show e2
    O_Plus e1 e2 -> show e1 ++ " + " ++ show e2
    O_Minus e1 e2 -> show e1 ++ " - " ++ show e2
    O_IsZero e -> "isZero(" ++ show e ++ ")"
    O_Mux sel YN{yes,no} -> show sel ++ " ? " ++ show yes ++ " : " ++ show no
    O_Reg (Reg _size id) -> show id
    O_Reg (Reg1 id) -> show id
    O_ReadRomByte romId a -> show romId ++ "[" ++ show a ++ "]"
    O_ReadRam ramId a -> show ramId ++ "[" ++ show a ++ "]"

instance Show a => Show (E a) where
  show = \case
    E_KeyDown key -> "pressed(" ++ show key ++ ")"
    E_Nat nat -> show (sizeOfNat nat) ++ "'" ++ printf "%X" (nat2int nat)
    E_Lit _ a -> show a
    E_Not e -> "!" ++ show e
    E_Tmp tmp -> show tmp
    E_TmpIndexed e i -> show e ++ "[" ++ show i ++ "]"
    E_Combine es -> "{" ++ show es ++ "}"

instance Show (Reg a) where
  show = \case
    Reg1 id -> show id
    Reg _size id -> show id ++ show _size

instance Show (Tmp a) where
  show = \case
    Tmp1 id -> show id
    Tmp _size id -> show id

deriving instance Show RomSpec

instance Show RegId where show RegId{u} = "r"++show u
instance Show TmpId where show TmpId{u} = "u"++show u
instance Show RomId where show RomId{u} = "rom"++show u
instance Show RamId where show RamId{u} = "ram"++show u
