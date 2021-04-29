
module Hdl_play (main) where

import Control.Monad (ap,liftM)
import Control.Monad.Fix (MonadFix,mfix)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn "*HDL*"
  runExample _example2

runExample :: (Container f, Functor f, Show (f Bit)) => H (f Wire) -> IO ()
runExample h = do
  let (s0,circuit@Circuit{drivers,nss},out) = elaborate h
  putStrLn("--------------------")
  mapM_ print drivers
  mapM_ print nss
  putStrLn("--------------------")
  let res = take 10 $ simForever circuit s0 out
  mapM_ print res

----------------------------------------------------------------------
-- design...

class Container f where -- TODO: Is this a good idea? Does it already exist
  elems :: f a -> [a]

_example1 :: H (Out1 Wire)
_example1 = do
  true <- Lit B1
  c0 <- flipper true
  pure $ Out1 c0

data Out1 a = Out1 a deriving (Show,Functor)

instance Container Out1 where
  elems (Out1 a) = [a]

_example2 :: H (Out2 Wire)
_example2 = do
  (x,y) <- twoBitCounter
  one <- andG x y
  two <- orG x y
  pure $ Out2 { inputs = [x,y], one, two }

data Out2 a = Out2 { inputs :: [a], one :: a, two :: a } deriving (Show,Functor)

instance Container Out2 where
  elems Out2{inputs,one,two} = inputs ++ [one,two]


twoBitCounter :: H (Wire,Wire)
twoBitCounter = do
  true <- Lit B1
  c0 <- flipper true
  c1 <- flipper c0
  pure (c1,c0)

flipper :: Wire -> H Wire
flipper en = mdo
  vbar <- notG v
  v <- dtype en vbar
  pure v

dtype :: Wire -> Wire -> H Wire
dtype en din = do
  WithNext B0 $ \v -> do
    v' <- mux en din v
    pure (v',v)

mux :: Wire -> Wire -> Wire -> H Wire
mux sel x y = do
  selb <- notG sel
  a <- andG sel x
  b <- andG selb y
  orG a b

orG :: Wire -> Wire -> H Wire
orG x y = do
  xb <- notG x
  yb <- notG y
  w <- andG xb yb
  notG w

andG :: Wire -> Wire -> H Wire
andG x y = do
  w <- Nand x y
  notG w

notG :: Wire -> H Wire
notG x = do
  Nand x x


----------------------------------------------------------------------
-- elaboration : HDL -> Circuit

instance Functor H where fmap = liftM
instance Applicative H where pure = return; (<*>) = ap
instance Monad H where return = Ret; (>>=) = Bind
instance MonadFix H where mfix = Mfix

data H a where
  Ret :: a -> H a
  Bind :: H a -> (a -> H b) -> H b
  Mfix :: (a -> H a) -> H a
  Lit :: Bit -> H Wire
  Nand :: Wire -> Wire -> H Wire
  WithNext :: Bit -> (Wire -> H (Wire,a)) -> H a

data Wire = Wire Int deriving (Ord,Eq,Show)

data State = State { ffMap :: Map FF Bit }
data FF = FF Int deriving (Ord,Eq,Show)

elaborate :: Container f => H (f Wire) -> (State,Circuit,f Wire)
elaborate h = do
  let
    loop :: ES -> H a -> (a,ES,[(FF,Bit)],[Driver],[NS])
    loop es0 = \case
      Ret x -> (x,es0,[],[],[])
      Bind h f -> do
        let (x,es1,inits1,dr1,ns1) = loop es0 h
        let (y,es2,inits2,dr2,ns2) = loop es1 (f x)
        (y,es2,inits1++inits2,dr1++dr2,ns1++ns2)
      Mfix f -> do
        let x@(a,_,_,_,_) = loop es0 (f a)
        x
      Lit val -> do
        let (wire,es1) = makeWire es0
        (wire,es1,
         [],
         [Driver { source = Const val, target = wire }],
         [])
      Nand x y -> do
        let (wire,es1) = makeWire es0
        (wire,es1,
         [],
         [Driver { source = NandGate (x,y), target = wire }],
         [])
      WithNext initVal f -> do
        let (ff,es1) = makeFF es0
        let (wire,es2) = makeWire es1
        let ((wire',a),es3,inits,dr,ns) = loop es2 (f wire)
        (a,es3,
         [(ff,initVal)] ++ inits,
         [Driver { source = ReadFF ff, target = wire}] ++ dr,
         [NS {source = wire', target = ff }] ++ ns)

  let (out,_,inits,drivers0,nss) = loop elabState0 h
  let s0 = State { ffMap = Map.fromList inits }
  let driverM = Map.fromList [ (target,source) | Driver{source,target} <- drivers0 ]
  let roots = elems out ++ [ w | NS{source=w} <- nss ]
  let deps w = driverRefs (look w driverM)
  let sorted = tsort roots deps
  let drivers = [ Driver { source = look w driverM, target = w } | w <- sorted ]
  let circuit = Circuit { drivers, nss }
  (s0,circuit,out)

makeFF :: ES -> (FF,ES)
makeFF es@ES{ff} = (FF ff,es { ff = ff + 1 })

makeWire :: ES -> (Wire,ES)
makeWire es@ES{w} = (Wire w,es { w = w + 1 })

data ES = ES { ff :: Int, w :: Int }
elabState0 :: ES
elabState0 = ES { ff = 661, w = 1 }

----------------------------------------------------------------------
-- circuit

data Circuit = Circuit { drivers :: [Driver], nss :: [NS] }

data Driver = Driver { source :: Source, target :: Wire }
  deriving Show

data NS = NS { source :: Wire, target :: FF }
  deriving Show

data Source
  = Const Bit
  | NandGate (Wire,Wire)
  | ReadFF FF
  deriving Show

driverRefs :: Source -> [Wire]
driverRefs = \case
  Const _ -> []
  NandGate (x,y) -> [x,y]
  ReadFF _ -> []

----------------------------------------------------------------------
-- circuit simulation

simForever :: forall f. Functor f => Circuit -> State -> f Wire -> [f Bit]
simForever circuit s0 outW = do
  let
    stepFrom :: State -> [f Bit]
    stepFrom s = do
      let (comb,s') = step circuit s
      let out = fmap (\w -> look w comb) outW
      [out] ++ stepFrom s'
  stepFrom s0


type Comb = Map Wire Bit

step :: Circuit -> State -> (Comb,State)
step Circuit{drivers,nss} State{ffMap=last} = do
  let
    evalSource :: Comb -> Source -> Bit
    evalSource comb = \case
      NandGate (x,y) -> nandBit (look x comb) (look y comb)
      Const bit -> bit
      ReadFF ff -> look ff last

    evalDriver :: Comb -> Driver -> Comb
    evalDriver comb Driver{source,target=w} =
      Map.insert w (evalSource comb source) comb

    loopDrivers :: Comb -> [Driver] -> Comb
    loopDrivers comb = \case
      [] -> comb
      x:xs -> loopDrivers (evalDriver comb x) xs

    comb = loopDrivers Map.empty drivers
    next = Map.fromList [ (ff, look w comb) | NS { source=w,target=ff} <- nss ]

  (comb,State {ffMap=next})


look :: (Show k,Ord k) => k -> Map k v -> v
look k m = maybe (error (show ("look:no-value!",k))) id $ Map.lookup k m


----------------------------------------------------------------------
-- bits

data Bit = B1 | B0 deriving (Show)

nandBit :: Bit -> Bit -> Bit
nandBit = \case
  B0 -> \_ -> B1
  B1 -> \case B0 -> B1; B1 -> B0

----------------------------------------------------------------------
-- generic tsort

tsort :: forall a. (Eq a, Ord a) => [a] -> (a -> [a]) -> [a]
tsort roots deps = walks [] Set.empty roots $ \_ -> []
  where

    walks :: [a] -> Set a -> [a] -> (Set a -> [a]) -> [a]
    walks doing done xs k = case xs of
      [] -> k done
      x:xs -> do
        walk doing done x $ \done ->
          walks doing done xs k

    walk :: [a] -> Set a -> a -> (Set a -> [a]) -> [a]
    walk doing done x k = if
      | x `elem` doing -> error "cycle"
      | x `elem` done -> k done
      | otherwise ->
          walks (x:doing) done (deps x) $ \done -> x : k (Set.insert x done)
