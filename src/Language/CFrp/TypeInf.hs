{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
module Language.CFrp.TypeInf (
    TypeInfError(..)
  , typeInf
  ) where

import Control.Applicative
import Control.Monad (zipWithM_)
import Control.Monad.Base (liftBase)
import Control.Monad.ST
import Control.Monad.Trans.Error
import Data.STRef
import qualified Data.Map as M
import qualified Data.HashTable.Class as HT
import Data.HashTable.ST.Basic (HashTable)
-- import Debug.Trace

import qualified Language.CFrp.Compat as C
import Language.CFrp.Syntax

data TypeInfError
  = UndefinedVariableError String
  | OccursCheckError
  | UnificationError Type Type
  deriving (Show, Eq)
instance Error TypeInfError

type TypeInf s = ErrorT TypeInfError (ST s)

initialEnv :: M.Map String Type
initialEnv = M.fromList $
  makeLiftN 9 ++
  [ ("foldp", FunT (FunT (VarT 1) (FunT (VarT 2) (VarT 2)))
                (FunT (VarT 2)
                  (FunT (SigT (VarT 1)) (SigT (VarT 2)))))
  ]
  where
    makeLiftN :: Int -> [(String, Type)]
    makeLiftN 0 = [("lift0", FunT (VarT 1) (SigT (VarT 1)))]
    makeLiftN n =
      let xs@((_, FunT a b):_) = makeLiftN (n-1)
      in ("lift" ++ show n, FunT (FunT (VarT (n+1)) a) (FunT (SigT (VarT (n+1))) b)):xs

typeInf :: M.Map String Type -> ParsedExpr -> Either TypeInfError TypedExpr
typeInf inputEnv t = runST $ do
  counter <- newSTRef 1
  tbl <- HT.new
  runErrorT $ do
    ve <- typeInf' counter tbl (M.union inputEnv initialEnv) M.empty t
    -- liftBase (HT.toList tbl) >>= \debug -> traceShow debug $ return ()
    generalize tbl M.empty ve

typeInf' :: forall s. STRef s Int -> (HashTable s Int Type) -> M.Map String Type -> M.Map String Type -> ParsedExpr -> TypeInf s TypedExpr
typeInf' counter tbl = go
  where
    go :: M.Map String Type -> M.Map String Type -> ParsedExpr -> TypeInf s TypedExpr
    go _ _ (UnitE _) = return $ UnitE UnitT
    go _ _ (IntE n _) = return $ IntE n IntT
    go env venv (AddE x y _) = arithOp env venv AddE x y
    go env venv (SubE x y _) = arithOp env venv SubE x y
    go env venv (MulE x y _) = arithOp env venv MulE x y
    go env venv (DivE x y _) = arithOp env venv DivE x y
    go env venv (EqE x y _) = compOp env venv EqE x y
    go env venv (LtE x y _) = compOp env venv LtE x y
    go env venv (AbsE var e _) = do
      tv <- newTypeVariable
      te <- go env (M.insert var tv venv) e
      return $ AbsE var te (FunT tv (exprTag te))
    go env venv (VarE var _) =
      case M.lookup var venv of
        Just t -> return $ VarE var t
        Nothing ->
          case M.lookup var env of
            Just t -> do
              t' <- instantiate t
              return $ VarE var t'
            Nothing -> throwError $ UndefinedVariableError var
    go env venv (AppE func args _) = do
      tfunc <- go env venv func
      targs <- mapM (go env venv) args
      tv <- newTypeVariable
      unify (exprTag tfunc) (foldr FunT tv (map exprTag targs))
      return $ AppE tfunc targs tv
    go env venv (TupE tuple _) = do
      ttuple <- mapM (go env venv) tuple
      return $ TupE ttuple (TupT (map exprTag ttuple))
    go env venv (IfE e1 e2 e3 _) = do
      te1 <- go env venv e1
      unify (exprTag te1) BoolT
      te2 <- go env venv e2
      te3 <- go env venv e3
      unify (exprTag te2) (exprTag te3)
      return $ IfE te1 te2 te3 (exprTag te2)
    go env venv (LetE var e1 e2 _) = do
      te1 <- go env venv e1
      te1' <- generalize tbl venv te1
      te2 <- go (M.insert var (exprTag te1') env) venv e2
      return $ LetE var te1' te2 (exprTag te2)
    go env venv (SeqE e1 e2 _) = do
      te1 <- go env venv e1
      -- unify (exprTag te1) UnitT
      te2 <- go env venv e2
      return $ SeqE te1 te2 (exprTag te2)

    arithOp env venv op x y = do
      tx <- go env venv x
      ty <- go env venv y
      unify (exprTag tx) IntT
      unify (exprTag ty) IntT
      return $ op tx ty IntT

    compOp env venv op x y = do
      tx <- go env venv x
      ty <- go env venv y
      unify (exprTag tx) IntT
      unify (exprTag ty) IntT
      return $ op tx ty BoolT

    newTypeVariable :: TypeInf s Type
    newTypeVariable = do
      n <- liftBase $ readSTRef counter
      liftBase $ C.modifySTRef' counter (+1)
      let v = VarT (-n)
      liftBase $ HT.insert tbl (-n) v
      return v

    instantiate :: Type -> TypeInf s Type
    instantiate typ = do
      ht <- liftBase HT.new
      work ht typ
      where
        work :: HashTable s Int Type -> Type -> TypeInf s Type
        work ht t@(VarT n)
          | n > 0 = do
              -- instantiate polymorphic type variable
              mt <- liftBase $ HT.lookup ht n
              case mt of
                Nothing -> do
                  t' <- newTypeVariable
                  liftBase $ HT.insert ht n t'
                  return t'
                Just t' -> return t'
          | otherwise = return t
        work ht (SigT t) = SigT <$> work ht t
        work ht (FunT t1 t2) = FunT <$> work ht t1 <*> work ht t2
        work ht (TupT ts) = TupT <$> mapM (work ht) ts
        work _ t = return t

    unify :: Type -> Type -> TypeInf s ()
    unify (VarT n1) (VarT n2) = do
      Just t1 <- liftBase $ HT.lookup tbl n1
      Just t2 <- liftBase $ HT.lookup tbl n2
      case (t1, t2) of
        -- XXX: inefficient
        (VarT n1', VarT n2') | n1 == n1' && n2 == n2' -> liftBase $ HT.insert tbl n1' t2
        _ -> unify t1 t2
    unify t1 t2@(VarT _) = unify t2 t1
    unify (VarT n1) t2 = do
      Just t1 <- liftBase $ HT.lookup tbl n1
      case t1 of
        VarT n | n == n1 -> do
          occursCheck n t2
          liftBase $ HT.insert tbl n t2
        _ -> unify t1 t2
    unify (FunT t11 t12) (FunT t21 t22) = unify t11 t21 >> unify t12 t22
    unify (SigT t1) (SigT t2) = unify t1 t2
    unify (TupT ts1) (TupT ts2) = zipWithM_ unify ts1 ts2
    unify t1 t2
      | t1 == t2 = return ()
      | otherwise = throwError $ UnificationError t1 t2

generalize :: forall s. (HashTable s Int Type) -> M.Map String Type -> TypedExpr -> TypeInf s TypedExpr
generalize tbl venv = go
  where
    go :: TypedExpr -> TypeInf s TypedExpr
    go (IntE n _) = return $ IntE n IntT
    go (UnitE _) = return $ UnitE UnitT
    go (AddE ve1 ve2 _) = arithOp ve1 ve2 AddE IntT
    go (SubE ve1 ve2 _) = arithOp ve1 ve2 SubE IntT
    go (MulE ve1 ve2 _) = arithOp ve1 ve2 MulE IntT
    go (DivE ve1 ve2 _) = arithOp ve1 ve2 DivE IntT
    go (EqE ve1 ve2 _) = arithOp ve1 ve2 EqE BoolT
    go (LtE ve1 ve2 _) = arithOp ve1 ve2 LtE BoolT
    go (IfE ve1 ve2 ve3 _) = do
      te1 <- go ve1
      te2 <- go ve2
      te3 <- go ve3
      return $ IfE te1 te2 te3 (exprTag te2)
    go (VarE var t)
      | Just t' <- M.lookup var venv = return $ VarE var t'
      | otherwise = VarE var <$> resolve t
    go (AbsE var e t) = do
      te <- go e
      let FunT t' _ = t
      t'' <- resolve t'
      return $ AbsE var te (FunT t'' (exprTag te))
    go (AppE func args _) = do
      func' <- go func
      args' <- mapM go args
      return $ AppE func' args' (removeArgs (exprTag func') (map exprTag args'))
      where
        removeArgs :: Type -> [Type] -> Type
        removeArgs f [] = f
        removeArgs (FunT t1 t2) (a:as)
          | t1 == a = removeArgs t2 as
          | otherwise = error $ "removeArgs: arg type mismatch?: " ++ show t1 ++ " " ++ show t2
        removeArgs t as = error $ "removeArgs: too many args?: " ++ show t ++ " " ++ show as
    go (TupE tuple _) = do
      ttuple <- mapM go tuple
      return $ TupE ttuple (TupT $ map exprTag ttuple)
    go (LetE var e1 e2 _) = do
      te1 <- go e1
      te2 <- go e2
      return $ LetE var te1 te2 (exprTag te2)
    go (SeqE e1 e2 _) = do
      te1 <- go e1
      te2 <- go e2
      return $ SeqE te1 te2 (exprTag te2)

    arithOp ve1 ve2 op ty = do
      te1 <- go ve1
      te2 <- go ve2
      return $ op te1 te2 ty

    resolve :: Type -> TypeInf s Type
    resolve (FunT t1 t2) = FunT <$> resolve t1 <*> resolve t2
    resolve (SigT t) = SigT <$> resolve t
    resolve (TupT ts) = TupT <$> mapM resolve ts
    resolve t@(VarT n)
      | n > 0 = return t
      | otherwise = do
        Just m <- liftBase $ HT.lookup tbl n
        case m of
          VarT n'
            | n' == n -> return $ VarT (-n)
            | otherwise -> resolve $ VarT n'
          _ -> resolve m
    resolve t = return t

occursCheck :: Int -> Type -> TypeInf s ()
occursCheck _ BoolT = return ()
occursCheck _ IntT = return ()
occursCheck _ UnitT = return ()
occursCheck n (FunT t1 t2) = occursCheck n t1 >> occursCheck n t2
occursCheck n (SigT t) = occursCheck n t
occursCheck n (TupT ts) = mapM_ (occursCheck n) ts
occursCheck n1 (VarT n2)
  | n1 == n2 = throwError OccursCheckError
  | otherwise = return ()
