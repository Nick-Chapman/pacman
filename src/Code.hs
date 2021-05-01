module Code (
  Code(..), Prog(..), Step(..), E(..), Oper(..), eNot,
  RegId, Reg(..), Tmp(..), TmpId(..), RomId, RomSpec(..),
  pretty,
  init, Context, State, runForOneFrame, Keys(..), Picture(..),
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Prelude hiding (init)
import Rom (Rom)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Rom (load,lookup)

import Value

-- full generated code. includes decs & prog
data Code = Code
  { regDecs :: [(RegId,Size)]
  , romSpecs :: [(RomId,RomSpec)]
  , entry :: Prog
  }

-- statement in the generated program, works in context of some decs
data Prog where
  P_Halt :: Prog
  P_Seq :: Step -> Prog -> Prog
  P_If :: E Bit -> Prog -> Prog -> Prog

-- basic program step (statement), which is sequenced in a program
data Step where
  S_Let :: Show a => Tmp a -> Oper a -> Step
  S_SetReg :: Show a => Reg a -> E a -> Step
  S_SetPixel :: XY (E Nat) -> RGB (E Nat) -> Step

-- operation (non atomic/pure expression), will always be let-bound
-- these things MUST be name
data Oper a where
  O_Reg :: Reg a -> Oper a
  O_And :: E Bit -> E Bit -> Oper Bit
  O_Plus :: E Nat -> E Nat -> Oper Nat
  O_ReadRomByte :: RomId -> E Nat -> Oper Nat

-- program expressions; atomic/pure, so can be freely shared
-- knows it's size
-- these things must *not* be named
data E a where
  E_KeyDown :: Key -> E Bit
  E_Lit :: Size -> a -> E a
  E_Not :: E Bit -> E Bit
  E_Tmp :: Tmp a -> E a
  E_TmpIndexed :: Tmp [Bit] -> Int -> E Bit
  E_Combine :: [E Bit] -> E [Bit]

-- TODO: break E into two levels E/A, with no recursion in E for Concat etc

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
  Tmp1 :: TmpId -> Tmp Bit -- TODO: remove for less cases?

data RomSpec = RomSpec { path :: String, size :: Int }

newtype RegId = RegId { u :: Int } deriving (Eq,Ord,Num)
newtype TmpId = TmpId { u :: Int } deriving (Eq,Ord)
newtype RomId = RomId { u :: Int } deriving (Eq,Ord,Num)

data Context = Context
  { roms :: Map RomId Rom
  }

-- everything which can form a saved-state
data State = State
  { regs :: Map RegId [Bit]
  -- TODO: rams will go here
  }

newtype Keys = Keys { pressed :: Set Key }

data Picture where
  Draw :: XY Int -> RGB Int -> Picture
  Pictures :: [Picture] -> Picture

init :: Code -> IO (Context,State,Prog) -- IO to load roms from file
init Code{entry=prog,regDecs,romSpecs} = do
  -- TODO: create ram from ram-spec
  let regs = Map.fromList [ (r,zeroOf size) | (r,Size {size}) <- regDecs ]
  roms <-
    Map.fromList <$> sequence
    [ do rom <- Rom.load size path; pure $ (id,rom)
    | (id,RomSpec {path,size}) <- romSpecs
    ]
  pure $ (Context {roms},State {regs},prog)

-- make no use of IO, but nice for debug
runForOneFrame :: Prog -> Context -> State -> Keys -> IO (Picture,State)
runForOneFrame prog context state keys = do
  let rs = RS { context, state, keys, screen = screen0, tmps = Map.empty }
  pure $ runProg rs prog

data RS = RS
  { context :: Context
  , keys :: Keys
  , state :: State
  , screen :: Screen
  , tmps :: Tmps
  }

type Tmps = Map TmpId [Bit]

runProg :: RS -> Prog -> (Picture,State)
runProg rs@RS{screen,state} = \case
  P_Halt -> (pictureScreen screen, state)
  P_Seq step prog -> do
    let rs' = evalStep rs step
    runProg rs' prog
  P_If cond this that -> do
    case evalE rs cond of
      B1 -> runProg rs this
      B0 -> runProg rs that

evalStep :: RS -> Step -> RS
evalStep rs@RS{screen,state=s@State{regs},tmps} = \case
  S_Let (Tmp1 id) oper ->
    rs { tmps = update tmps id [evalOper rs oper] }

  S_Let (Tmp size id) oper -> do
    rs { tmps = update tmps id (checkSize size (evalOper rs oper)) }

  S_SetReg (Reg1 id) e ->
    rs { state = s { regs = update regs id [evalE rs e] } }

  S_SetReg (Reg size id) e -> do
    rs { state = s { regs = update regs id (checkSize size (evalE rs e)) } }

  S_SetPixel xy rgb -> do
    rs { screen =
         setScreenPixel
         screen
         (fmap (fromBits . evalE rs) xy)
         (fmap (fromBits . evalE rs) rgb)
       }

evalOper :: RS -> Oper a -> a
evalOper rs@RS{context=Context{roms},state=State{regs}} = \case
  O_And e1 e2 -> andBit (evalE rs e1) (evalE rs e2)
  O_Plus e1 e2 -> plusBits (evalE rs e1) (evalE rs e2)
  O_Reg (Reg size id) -> do
    checkSize size (look regs id)
  O_Reg (Reg1 id) ->
    case (look regs id) of
      [b] -> b
      bits -> error (show ("evalE/Reg1",id,length bits))
  --O_Exp e -> evalE rs e
  O_ReadRomByte romId a ->
    bitsOfInt (Size 8) (fromIntegral (Rom.lookup (look roms romId) (fromBits (evalE rs a))))

evalE :: RS -> E a -> a
evalE rs@RS{keys=Keys{pressed}} = \case
  E_KeyDown key -> if Set.member key pressed then B1 else B0
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
pictureScreen Screen{m} = Pictures [ Draw (shift xy) rgb | (xy,rgb) <- Map.toList m ]
  where
    shift XY{x,y} = XY { x = x+8, y = y+8 }

----------------------------------------------------------------------
-- show

instance Show Step where
  show = \case
    S_Let tmp oper -> "let " ++ show tmp ++ " = " ++ show oper
    S_SetReg reg exp -> show reg ++ " := " ++ show exp
    S_SetPixel xy rgb -> "set-pixel " ++ show xy ++ " := " ++ show rgb

deriving instance Show (Oper a)
deriving instance Show a => Show (E a)

instance Show (Reg a) where
  show = \case
    Reg1 id -> show id
    Reg size id -> show id ++ show size

instance Show (Tmp a) where
  show = \case
    Tmp1 id -> show id
    Tmp size id -> show id ++ show size

instance Show RegId where show RegId{u} = "r"++show u
instance Show TmpId where show TmpId{u} = "u"++show u
instance Show RomId where show RomId{u} = "rom"++show u

pretty :: Pretty a => a -> String
pretty = unlines . lay

class Pretty a where
  lay :: a -> [String]

instance Pretty Code where
  lay Code{regDecs,entry} =
    [ "reg " ++ show r ++ " : " ++ show s | (r,s) <- regDecs ]
    ++ lay entry

instance Pretty Prog where
  lay = \case
    P_Halt -> []
    P_Seq step prog -> lay step ++ lay prog
    P_If cond this that ->
      ["if (" ++ show cond ++ ") {"]
      ++ tab (lay this)
      ++ ["} else {"]
      ++ tab (lay that) ++ ["}"]

instance Pretty Step where lay x = [show x]

tab :: [String] -> [String]
tab xs = [ "  " ++ x | x <- xs ]
