module Elaborate(elab) where

import Types (
  -- source
  System(..),Eff(..),
  -- target
  Code(..),Prog(..),Step(..),E(..),
  -- runtime
  UpDown(..),
  )

elab :: System -> Code
elab = loop
  where
    loop :: System -> Code
    loop = \case
      FrameEffect eff -> do
        let prog = compile0 eff
        Code { mm = [], regs = [], entry = prog }

compile0 :: Eff () -> Prog
compile0 eff0 = comp eff0 (\() -> P_Halt)
  where
    comp :: Eff a -> (a -> Prog) -> Prog
    comp eff k = case eff of
      Ret a -> k a
      Bind e f -> comp e $ \a -> comp (f a) k
      GetKey key -> P_If (E_KeyDown key) (k Down) (k Up)
      SetPixel xy rgb -> do P_Seq (S_SetPixel xy rgb) (k ())
