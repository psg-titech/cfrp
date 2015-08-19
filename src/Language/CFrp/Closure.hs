{-# LANGUAGE ScopedTypeVariables #-}
module Language.CFrp.Closure (
    closureConvert
  ) where

import Control.Applicative
import Control.Monad.ST (ST, runST)
import Data.Function (on)
import Data.Monoid ((<>), mconcat)
import Data.STRef
import qualified Data.Set as S

import qualified Language.CFrp.Compat as C
import Language.CFrp.Syntax

closureConvert :: TypedKExpr -> Program
closureConvert tkexpr = runST $ do
  counter <- newSTRef 1
  clsdefs <- newSTRef []
  r <- closureConvert' counter clsdefs tkexpr
  Program <$> readSTRef clsdefs <*> pure r

closureConvert' :: forall s. STRef s Int -> STRef s [ClsDef] -> TypedKExpr -> ST s TypedCExpr
closureConvert' counter clsdefs = go
  where
    gensym :: ST s Int
    gensym = do
      n <- readSTRef counter
      C.modifySTRef' counter (+1)
      return (-n)

    go :: TypedKExpr -> ST s TypedCExpr
    go (PrimK k) = return $ PrimC $ convPrim k
    go (LetK x k1 k2 t) = LetC x <$> go k1 <*> go k2 <*> pure t
    go (SeqK k1 k2 t) = SeqC <$> go k1 <*> go k2 <*> pure t
    go (AbsK x k t) = do
      c <- go k
      PrimC <$> defineFunction x c (freeVariables x k) t
    go (TupK ks t) = return $ TupC (map convPrim ks) t
    go (AppK k1 ks t) = return $ AppC (convPrim k1) (map convPrim ks) t
    go (AddK k1 k2 t) = arithOp AddC k1 k2 t
    go (SubK k1 k2 t) = arithOp SubC k1 k2 t
    go (MulK k1 k2 t) = arithOp MulC k1 k2 t
    go (DivK k1 k2 t) = arithOp DivC k1 k2 t
    go (EqK k1 k2 t) = arithOp EqC k1 k2 t
    go (LtK k1 k2 t) = arithOp LtC k1 k2 t
    go (IfK k1 k2 k3 t) = IfC <$> pure (convPrim k1) <*> go k2 <*> go k3 <*> pure t

    arithOp :: (CPrim a -> CPrim a -> a -> CExpr a) -> KPrim a -> KPrim a -> a -> ST s (CExpr a)
    arithOp op k1 k2 t = return $ op (convPrim k1) (convPrim k2) t

    defineFunction :: KVar -> TypedCExpr -> [(String, Type)] -> Type -> ST s TypedCPrim
    defineFunction x c fvs typ = do
      label <- gensym
      let FunT argType _ = typ
      let clsdef = ClsDef {
          clsName = label
        , clsFreeVars = fvs
        , clsArg = x
        , clsArgType = argType
        , clsBody = c
        , clsBodyType = cexprTag c
        }
      C.modifySTRef' clsdefs (clsdef:)
      return $ ClsC label fvs typ

convPrim :: KPrim a -> CPrim a
convPrim (UnitK t) = UnitC t
convPrim (IntK n t) = IntC n t
convPrim (VarK x t) = VarC x t

newtype P a b = P { unP :: (a, b) }
instance Eq a => Eq (P a b) where
  (==) = (==) `on` (fst . unP)
instance Ord a => Ord (P a b) where
  compare = compare `on` (fst . unP)

freeVariables :: KVar -> KExpr a -> [(String, a)]
freeVariables arg = map unP . S.toList . go (initial arg)
  where
    initial (KVarS s) = S.singleton s
    initial (KVarI _) = S.empty

    go :: S.Set String -> KExpr a -> S.Set (P String a)
    go bound (PrimK p) = goPrim bound p
    go bound (AddK k1 k2 _) = goPrim' bound [k1, k2]
    go bound (SubK k1 k2 _) = goPrim' bound [k1, k2]
    go bound (MulK k1 k2 _) = goPrim' bound [k1, k2]
    go bound (DivK k1 k2 _) = goPrim' bound [k1, k2]
    go bound (EqK k1 k2 _) = goPrim' bound [k1, k2]
    go bound (LtK k1 k2 _) = goPrim' bound [k1, k2]
    go bound (AbsK (KVarS s) k _) = go (S.insert s bound) k
    go bound (AbsK (KVarI _) k _) = go bound k
    go bound (AppK k1 ks _) = goPrim' bound (k1:ks)
    go bound (IfK k1 k2 k3 _) = goPrim bound k1 <> go bound k2 <> go bound k3
    go bound (LetK (KVarS s) k1 k2 _) = go bound k1 <> go (S.insert s bound) k2
    go bound (LetK (KVarI _) k1 k2 _) = go bound k1 <> go bound k2
    go bound (TupK ks _) = goPrim' bound ks
    go bound (SeqK k1 k2 _) = go bound k1 <> go bound k2

    goPrim :: S.Set String -> KPrim a -> S.Set (P String a)
    goPrim bound (VarK (KVarS s) t)
      | s `S.member` bound = S.empty
      | otherwise = S.singleton $ P (s, t)
    goPrim _ _ = S.empty

    goPrim' :: S.Set String -> [KPrim a] -> S.Set (P String a)
    goPrim' bound = mconcat . map (goPrim bound)
