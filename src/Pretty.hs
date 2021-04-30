
module Pretty (pretty) where

import Types (Code(..),Prog(..),Step)

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
      ["if (" ++ show cond ++ ") {"] ++ tab (lay this) ++ ["} else {"] ++ tab (lay that) ++ ["}"]

instance Pretty Step where lay x = [show x]

tab :: [String] -> [String]
tab xs = [ "  " ++ x | x <- xs ]
