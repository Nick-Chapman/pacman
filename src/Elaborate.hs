module Elaborate(elab) where -- TODO: rename Compile

import Types (
  -- source
  System(..),Eff(..),
  -- target
  Code(..),Prog(..),Step(..),E(..),Oper(..),
  RegId(..), SizeSpec(..), Reg(..),
  TmpId(..), Tmp(..),
  -- runtime
  Bit(..),
  )

elab :: System -> Code
elab = loop ES { u = 0, regs = [] }
  where
    loop :: ES -> System -> Code
    loop es@ES{u,regs} = \case
      DeclareReg1 f -> do
        let regId = RegId { u }
        let regSpec = SizeSpec { size = 1 }
        let reg = Reg1 regId
        loop es { u = u + 1, regs = (regId,regSpec) : regs } (f reg)
      FrameEffect eff -> do
        let prog = compile0 eff
        Code { regDecs = regs, entry = prog }

data ES = ES -- elab state
  { u :: Int
  , regs :: [(RegId,SizeSpec)]
  }

compile0 :: Eff () -> Prog
compile0 eff0 = comp CS { u = 0 } eff0 (\_ () -> P_Halt)
  where
    comp :: CS -> Eff a -> (CS -> a -> Prog) -> Prog
    comp s@CS{u} eff k = case eff of
      Ret a -> k s a
      Bind e f -> comp s e $ \s a -> comp s (f a) k
      CaseBit bit -> P_If (E_TestBit bit) (k s B1) (k s B0) -- dup s, re-uses tmp u, ok?
      KeyDown key -> k s (E_KeyDown key)
      SetPixel xy rgb -> P_Seq (S_SetPixel xy rgb) (k s ())
      GetReg reg -> k s (E_Reg reg)
      SetReg reg exp -> P_Seq (S_SetReg reg exp) (k s ())
      Not e -> k s (E_Not e)
      And e1 e2 -> do
        let tmpId = TmpId { u }
        let tmp = Tmp1 tmpId
        let oper = O_And e1 e2
        P_Seq (S_Let tmp oper) (k s { u = u + 1} (E_Tmp tmp))

data CS = CS
  { u :: Int -- TODO: contiue to use same u as for reg, to unify tmp/reg-id
  }
