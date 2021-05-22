module OneFrame (main) where

import Code (Code,State,Keys(..),Picture(..))
import Prelude hiding (init)
import Value (XY(..),RGB(..))
import qualified Code (initialize,runForOneFrame)
import qualified Data.Set as Set

main :: String -> Code -> IO ()
main name code = do
  let max = 1
  putStrLn $ "*OneFrame*  max=" ++ show max
  (_,context,state,prog) <- Code.initialize code
  let keys = Keys { pressed = Set.empty }
  let
    loop :: Int -> State -> IO ()
    loop i state = do
      if i == max then pure () else do
        putStrLn $ "emulating frame #" ++ show i
        (picture,state) <- Code.runForOneFrame prog context state keys
        generateFile (name++"-frame-"++show i++".txt") (PixelsOf picture)
        loop (i+1) state

  loop 0 state

generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let str = show a
  let nlines = length [ () | '\n' <- str ]
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp <> " (" ++ show nlines ++ " lines)"
  writeFile fp str

newtype PixelsOf = PixelsOf { picture :: Picture }

instance Show PixelsOf where
  show = unlines . map show . flat . picture
    where
      flat :: Picture -> [(XY Int,RGB Int)]
      flat = \case
        Draw xy rgb -> [(xy,rgb)]
        Pictures xs -> xs >>= flat
