module Compile(elab) where

import Types (
  -- source
  System(..),Eff(..),
  -- target
  Code(..),Prog(..),Step(..),E(..),Oper(..),
  RomSpec(..),RomId(..),
  RegId(..), SizeSpec(..), Reg(..),
  TmpId(..), Tmp(..),
  -- runtime
  Bit(..),
  size1,
  )

elab :: System -> Code
elab = loop ES { regId = 0, regs = [], romId = 101, roms = [] }
  where
    loop :: ES -> System -> Code
    loop es@ES{regId,regs,romId,roms} = \case
      DeclareRom spec f -> do
        loop es { romId = romId + 1, roms = (romId,spec) : roms } (f romId)
      DeclareReg size f -> do
        let reg = Reg size regId
        loop es { regId = regId + 1, regs = (regId,size) : regs } (f reg)
      DeclareReg1 f -> do
        let size = SizeSpec { size = 1 }
        let reg = Reg1 regId
        loop es { regId = regId + 1, regs = (regId,size) : regs } (f reg)
      FrameEffect eff -> do
        let prog = compile0 eff
        Code { romSpecs = roms, regDecs = regs, entry = prog }

data ES = ES -- elab state
  { regId :: RegId
  , regs :: [(RegId,SizeSpec)]
  , romId :: RomId
  , roms :: [(RomId,RomSpec)]
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
          E_Lit _ B1 -> k s B1
          E_Lit _ B0 -> k s B0
          _ -> P_If bit (k s B1) (k s B0) -- dup s, re-uses tmp u, ok?

      GetReg reg@(Reg size _) -> do
        let id = TmpId { u }
        let tmp = Tmp size id
        let oper = O_Reg reg
        P_Seq (S_Let tmp oper) $
          k s { u = u + 1 } (E_Tmp tmp)

      GetReg reg@Reg1{} -> do
        let tmpId = TmpId { u }
        let tmp = Tmp1 tmpId
        let oper = O_Reg reg
        P_Seq (S_Let tmp oper) $
          k s { u = u + 1 } (E_Tmp tmp)

      Plus e1 e2 -> do
        let tmpId = TmpId { u }
        let size = max (sizeE e1) (sizeE e2)
        let tmp = Tmp size tmpId
        let oper = O_Plus e1 e2
        P_Seq (S_Let tmp oper) $
          k s { u = u + 1} (E_Tmp tmp)

      And e1 e2 -> do
        let tmpId = TmpId { u }
        let tmp = Tmp1 tmpId
        let oper = O_And e1 e2
        P_Seq (S_Let tmp oper) $
          k s { u = u + 1} (E_Tmp tmp)

      --ReadRomByte rid a -> do k s (E_ReadRomByte rid a)
      ReadRomByte rid a -> do
        let tmpId = TmpId { u }
        let size = SizeSpec 8
        let tmp = Tmp size tmpId
        let oper = O_ReadRomByte rid a
        P_Seq (S_Let tmp oper) $
          k s (E_Tmp tmp)

      Index e i -> do
        let tmpId = TmpId { u }
        let size = sizeE e
        let tmp = Tmp size tmpId
        let oper = O_Exp e --undefined e i -- O_Index e i
        P_Seq (S_Let tmp oper) $
          k s { u = u + 1 } (E_TmpIndexed tmp i)

      Split e -> do
        let tmpId = TmpId { u }
        let size@(SizeSpec n) = sizeE e
        let tmp = Tmp size tmpId
        let oper = O_Exp e
        P_Seq (S_Let tmp oper) $
          k s { u = u + 1 } $ [E_TmpIndexed tmp i | i <- [0..n-1]]

      -- TODO: factor duplicate code above which introduces tmps for various opers

      Combine e -> do
        k s (E_Combine e)


sizeE :: E a -> SizeSpec
sizeE = \case
  --E_ReadRomByte{} -> SizeSpec 8
  E_KeyDown{} -> SizeSpec 1
  E_Lit size _ -> size
  E_Not e -> sizeE e
  E_Tmp (Tmp size _) -> size
  E_Tmp (Tmp1 _) -> SizeSpec 1
  E_TmpIndexed _ _ -> size1
  E_Combine es -> sum [ sizeE e | e <- es ]

data CS = CS
  { u :: Int -- TODO: contiue to use same u as for reg, to unify tmp/reg-id
  }
