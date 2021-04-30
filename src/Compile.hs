module Compile(elab) where

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

      SetPixel xy rgb -> P_Seq (S_SetPixel xy rgb) (k s ())
      SetReg reg exp -> P_Seq (S_SetReg reg exp) (k s ())

      CaseBit bit -> do
        case bit of
          E_Lit B1 -> k s B1
          E_Lit B0 -> k s B0
          _ -> P_If bit (k s B1) (k s B0) -- dup s, re-uses tmp u, ok?

      GetReg reg@Reg1{} -> do
        let tmpId = TmpId { u }
        let tmp = Tmp1 tmpId
        let oper = O_Reg reg
        P_Seq (S_Let tmp oper) $
          k s { u = u + 1 } (E_Tmp tmp)

      And e1 e2 -> do
        let tmpId = TmpId { u }
        let tmp = Tmp1 tmpId
        let oper = O_And e1 e2
        P_Seq (S_Let tmp oper) $
          k s { u = u + 1} (E_Tmp tmp)

data CS = CS
  { u :: Int -- TODO: contiue to use same u as for reg, to unify tmp/reg-id
  }
