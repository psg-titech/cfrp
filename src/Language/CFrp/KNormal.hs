{-# LANGUAGE ScopedTypeVariables #-}
module Language.CFrp.KNormal (
  kNormalize
) where

import Control.Applicative
import Control.Monad.ST (ST, runST)
import Data.STRef

import qualified Language.CFrp.Compat as C
import Language.CFrp.Syntax

kNormalize :: TypedExpr -> TypedKExpr
kNormalize te = runST $ do
  counter <- newSTRef 0
  kNormalize' counter te

kNormalize' :: forall s. STRef s Int -> TypedExpr -> ST s TypedKExpr
kNormalize' counter = go
  where
    gensym :: ST s KVar
    gensym = do
      n <- readSTRef counter
      C.modifySTRef' counter (+1)
      return $ KVarI n

    insertLet :: TypedKExpr -> (TypedKPrim -> ST s TypedKExpr) -> ST s TypedKExpr
    insertLet (PrimK x) k = k x
    insertLet e k = do
      v <- gensym
      k' <- k $ VarK v (kexprTag e)
      return $ LetK v e k' (kexprTag k')

    go :: TypedExpr -> ST s TypedKExpr
    go (UnitE t) = return $ PrimK (UnitK t)
    go (IntE n t) = return $ PrimK (IntK n t)
    go (VarE x t) = return $ PrimK (VarK (KVarS x) t)
    go (TupE es t) = do
      ks <- mapM go es
      f ks $ \ks' -> return $ TupK ks' t
    go (AddE e1 e2 t) = arithOp AddK e1 e2 t
    go (SubE e1 e2 t) = arithOp SubK e1 e2 t
    go (MulE e1 e2 t) = arithOp MulK e1 e2 t
    go (DivE e1 e2 t) = arithOp DivK e1 e2 t
    go (EqE e1 e2 t) = arithOp EqK e1 e2 t
    go (LtE e1 e2 t) = arithOp LtK e1 e2 t
    go (AbsE x e t) = AbsK (KVarS x) <$> go e <*> pure t
    go (AppE e1 es t) = do
      k1 <- go e1
      ks <- mapM go es
      insertLet k1 $ \k1' ->
        f ks $ \ks' ->
          return $ AppK k1' ks' t
    go (LetE x e1 e2 t) = LetK (KVarS x) <$> go e1 <*> go e2 <*> pure t
    go (SeqE e1 e2 t) = SeqK <$> go e1 <*> go e2 <*> pure t
    go (IfE e1 e2 e3 t) = do
      k1 <- go e1
      k2 <- go e2
      k3 <- go e3
      insertLet k1 $ \k1' ->
        return $ IfK k1' k2 k3 t

    arithOp op e1 e2 t = do
      k1 <- go e1
      k2 <- go e2
      insertLet k1 $ \k1' ->
        insertLet k2 $ \k2' ->
          return $ op k1' k2' t

    f :: [TypedKExpr] -> ([TypedKPrim] -> ST s TypedKExpr) -> ST s TypedKExpr
    f [] k = k []
    f (x:xs) k = insertLet x $ \x' ->
      f xs $ \xs' ->
        k (x':xs')
