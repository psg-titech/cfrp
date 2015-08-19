{-# LANGUAGE PatternGuards, RecordWildCards #-}
module Language.CFrp.CodeGen (codeGen, runtimeHeaderPath) where

import qualified Data.DList as D
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Paths_cfrp (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)

import Language.CFrp.Syntax

runtimeHeaderPath :: FilePath
runtimeHeaderPath = unsafePerformIO $ getDataFileName "runtime.h"

codeGen :: M.Map String String -> M.Map String String -> [String] -> Program -> [String]
codeGen inputEnv importEnv codes Program{..} = D.toList $
  D.fromList prelude
  <> D.fromList codes
  <> D.concat (map codeGenClosureDecl programClosures)
  <> D.concat (map codeGenClosureBody programClosures)
  <> codeGenMain inputEnv importEnv programMain
  where
    prelude = ["#include " <> show runtimeHeaderPath]

codeGenClosureDecl :: ClsDef -> D.DList String
codeGenClosureDecl ClsDef{..} =
  D.fromList
    [ "struct " <> makeClosureName clsName <> " : public cfrp::closure"
    , "{"
    ]
  <> D.fromList (map (\v -> "cfrp::value " <> v <> ";") freeVariables)
  <> D.fromList
      [ makeClosureName clsName <> "(" <> intercalate ", " (map (\v -> "cfrp::value " <> v <> "_") freeVariables) <> ");"
      , "cfrp::value operator()(cfrp::value " <> makeVarName clsArg <> ");"
      , "virtual void mark() const;"
      , "};"
      ]
  where
    freeVariables = map fst clsFreeVars

codeGenClosureBody :: ClsDef -> D.DList String
codeGenClosureBody ClsDef{..} =
   D.fromList
    [ makeClosureName clsName <> "::" <> makeClosureName clsName <> "(" <> intercalate ", " (map (\v -> "cfrp::value " <> v <> "_") freeVariables) <> ")"
    , generateInitializer freeVariables <> " {}"
    , ""
    , "cfrp::value " <> makeClosureName clsName <> "::operator()(cfrp::value " <> makeVarName clsArg <> ")"
    , "{"
    , "cfrp::value retval;"
    ]
  <> codeGenExpr clsBody
  <> D.fromList ["return retval;",  "}"]
  <> D.fromList ["void " <> makeClosureName clsName <> "::mark() const", "{"]
  <> D.fromList (foldr (\(v, t) -> genMarker v t) [] clsFreeVars)
  <> D.singleton "}"
  where
    freeVariables = map fst clsFreeVars

    generateInitializer [] = ""
    generateInitializer fvs = " : " <> intercalate ", " (map (\v -> v <> "(" <> v <> "_)") fvs)

    genMarker :: String -> Type -> [String] -> [String]
    genMarker _ UnitT acc = acc
    genMarker _ BoolT acc = acc
    genMarker _ IntT acc = acc
    genMarker v t@(SigT _) acc = mark v t : acc
    genMarker v t@(FunT _ _) acc = mark v t : acc
    genMarker v t@(VarT _) acc = mark v t : acc

    mark :: String -> Type -> String
    mark v t = "cfrp::global_memory->mark(" <> v <> "); // " <> show t

codeGenMain :: M.Map String String -> M.Map String String -> TypedCExpr -> D.DList String
codeGenMain inputEnv importEnv expr =
  D.fromList [ "cfrp::memory_manager *cfrp::global_memory;", "int main()" , "{", "cfrp::global_memory = new cfrp::memory_manager();"]
  <> D.fromList (M.foldrWithKey (\k v acc -> "cfrp::value " <> k <> " = new " <> v <> "();" : acc) [] inputEnv)
  <> D.fromList (map (\i -> "cfrp::value lift" <> show i <> " = new cfrp::lift_closure<" <> show i <> ">();") [1::Int .. 9])
  <> D.singleton "cfrp::value foldp = new cfrp::foldp_closure();"
  <> D.fromList (M.foldrWithKey (\k v acc -> "cfrp::value " <> k <> " = initializer::" <> v <> "();" : acc) [] importEnv)
  <> D.fromList
      [ "cfrp::value retval;"
      , "cfrp::engine engine;"
      ]
  <> D.fromList (map (\v -> "engine.register_input_node(" <> v <> ".node_value);") (M.keys inputEnv))
  <> codeGenExpr expr
  <> D.fromList
      [ "for (int i = 0; i < 1000; i++) {"
      , "cfrp::global_memory->garbage_collect();"
      , "engine.loop();"
      , "struct timespec req;"
      , "req.tv_sec = 0;"
      , "req.tv_nsec = 100 * 1000 * 1000;"
      , "nanosleep(&req, NULL);"
      , "}"
      , "return 0;"
      , "}"
      ]

codeGenExpr :: TypedCExpr -> D.DList String
codeGenExpr (PrimC p) = D.singleton $ "retval = " <> codeGenPrim p <> ";"
codeGenExpr (AddC p1 p2 _) = arithOp "+" p1 p2
codeGenExpr (SubC p1 p2 _) = arithOp "-" p1 p2
codeGenExpr (MulC p1 p2 _) = arithOp "*" p1 p2
codeGenExpr (DivC p1 p2 _) = arithOp "/" p1 p2
codeGenExpr (EqC p1 p2 _) = arithOp "==" p1 p2
codeGenExpr (LtC p1 p2 _) = arithOp "<" p1 p2
codeGenExpr (LetC v e1 e2 _) = codeGenExpr e1 <> D.singleton ("cfrp::value " <> makeVarName v <> " = retval;") <> codeGenExpr e2
codeGenExpr (SeqC e1 e2 _) = codeGenExpr e1 <> codeGenExpr e2
codeGenExpr (AppC p ps _) = D.singleton $ "retval = (*" <> codeGenPrim p <> ".closure_value)(" <> intercalate ", " (map codeGenPrim ps) <> ");"
codeGenExpr (IfC p e1 e2 _) =
  D.singleton ("if (" <> codeGenPrim p <> ".int_value) {")
  <> codeGenExpr e1
  <> D.singleton "} else {"
  <> codeGenExpr e2
  <> D.singleton "}"

arithOp :: String -> TypedCPrim -> TypedCPrim -> D.DList String
arithOp op p1 p2 = D.singleton $ "retval = " <> codeGenPrim p1 <> ".int_value " <> op <> " " <> codeGenPrim p2 <> ".int_value;"

codeGenPrim :: TypedCPrim -> String
codeGenPrim (UnitC _) = "cfrp::value()"
codeGenPrim (IntC n _) = "cfrp::value(" <> show n <> ")"
codeGenPrim (VarC v _) = makeVarName v
codeGenPrim (ClsC n fvs _) = "new " <> makeClosureName n <> "(" <> intercalate ", " (map fst fvs) <> ")"

makeVarName :: KVar -> String
makeVarName (KVarS s) = s
makeVarName (KVarI n) = "kvar_i_" <> show n

makeClosureName :: Int -> String
makeClosureName n = "closure_" <> show (-n)
