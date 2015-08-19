module Language.CFrp.Syntax where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Char (isAlpha, isAscii)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data TaggedExpr a
  = AddE (TaggedExpr a) (TaggedExpr a) a
  | SubE (TaggedExpr a) (TaggedExpr a) a
  | MulE (TaggedExpr a) (TaggedExpr a) a
  | DivE (TaggedExpr a) (TaggedExpr a) a
  | EqE (TaggedExpr a) (TaggedExpr a) a
  | LtE (TaggedExpr a) (TaggedExpr a) a
  | IntE Int a
  | VarE String a
  | AbsE String (TaggedExpr a) a
  | AppE (TaggedExpr a) [TaggedExpr a] a
  | TupE [TaggedExpr a] a
  | SeqE (TaggedExpr a) (TaggedExpr a) a
  | UnitE a
  | IfE (TaggedExpr a) (TaggedExpr a) (TaggedExpr a) a
  | LetE String (TaggedExpr a) (TaggedExpr a) a
  deriving (Show, Eq)
instance Arbitrary a => Arbitrary (TaggedExpr a) where
  arbitrary = sized $ gen predefined
    where
      predefined = ["foldp", "lift0", "lift1", "lift2"]
      gen vars 0 = oneof
        [ IntE <$> arbitrary <*> arbitrary
        , VarE <$> elements vars <*> arbitrary
        , UnitE <$> arbitrary
        ]
      gen vars n = oneof
        [ AddE <$> gen vars n2 <*> gen vars n2 <*> arbitrary
        , EqE <$> gen vars n2 <*> gen vars n2 <*> arbitrary
        , do { x <- readableString; AbsE x <$> gen (x:vars) (n-1) <*> arbitrary }
        , AppE <$> gen vars n2 <*> (pure <$> gen vars n2) <*> arbitrary
        , IfE <$> gen vars n3 <*> gen vars n3 <*> gen vars n3 <*> arbitrary
        , TupE <$> replicateM 3 (gen vars n3) <*> arbitrary
        , do { x <- readableString; LetE x <$> gen vars n2 <*> gen (x:vars) n2 <*> arbitrary }
        , SeqE <$> gen vars n2 <*> gen vars n2 <*> arbitrary
        ]
        where
          n2 = n `div` 2
          n3 = n `div` 3

      readableString = arbitrary `suchThat` (\s -> not (null s) && all (\c -> isAscii c && isAlpha c) s)

exprTag :: TaggedExpr a -> a
exprTag (AddE _ _ t) = t
exprTag (SubE _ _ t) = t
exprTag (MulE _ _ t) = t
exprTag (DivE _ _ t) = t
exprTag (EqE _ _ t) = t
exprTag (LtE _ _ t) = t
exprTag (IntE _ t) = t
exprTag (VarE _ t) = t
exprTag (AbsE _ _ t) = t
exprTag (AppE _ _ t) = t
exprTag (TupE _ t) = t
exprTag (SeqE _ _ t) = t
exprTag (UnitE t) = t
exprTag (IfE _ _ _ t) = t
exprTag (LetE _ _ _ t) = t

data TaggedDecl a
  = InputD String Type String a
  | ImportD String Type String a
  | EmbedD String a
  deriving (Show, Eq)

data Type
  = UnitT
  | BoolT
  | IntT
  | TupT [Type]
  | SigT Type
  | FunT Type Type
  | VarT Int
  deriving (Show, Eq)

type ParsedExpr = TaggedExpr ()
type ParsedDecl = TaggedDecl ()
type TypedExpr = TaggedExpr Type

data KVar = KVarS String | KVarI Int deriving (Show, Eq)

data KPrim a
  = UnitK a
  | IntK Int a
  | VarK KVar a
  deriving (Show, Eq)

kprimTag :: KPrim a -> a
kprimTag (UnitK a) = a
kprimTag (IntK _ a) = a
kprimTag (VarK _ a) = a

data KExpr a
  = PrimK (KPrim a)
  | AddK (KPrim a) (KPrim a) a
  | SubK (KPrim a) (KPrim a) a
  | MulK (KPrim a) (KPrim a) a
  | DivK (KPrim a) (KPrim a) a
  | EqK (KPrim a) (KPrim a) a
  | LtK (KPrim a) (KPrim a) a
  | AbsK KVar (KExpr a) a
  | AppK (KPrim a) [KPrim a] a
  | TupK [KPrim a] a
  | SeqK (KExpr a) (KExpr a) a
  | IfK (KPrim a) (KExpr a) (KExpr a) a
  | LetK KVar (KExpr a) (KExpr a) a
  deriving (Show, Eq)

kexprTag :: KExpr a -> a
kexprTag (PrimK x) = kprimTag x
kexprTag (AddK _ _ a) = a
kexprTag (SubK _ _ a) = a
kexprTag (MulK _ _ a) = a
kexprTag (DivK _ _ a) = a
kexprTag (EqK _ _ a) = a
kexprTag (LtK _ _ a) = a
kexprTag (AbsK _ _ a) = a
kexprTag (AppK _ _ a) = a
kexprTag (TupK _ a) = a
kexprTag (SeqK _ _ a) = a
kexprTag (IfK _ _ _ a) = a
kexprTag (LetK _ _ _ a) = a

type TypedKPrim = KPrim Type
type TypedKExpr = KExpr Type

data CPrim a
  = UnitC a
  | IntC Int a
  | VarC KVar a
  | ClsC Int [(String, Type)] a
  deriving (Show, Eq)

data CExpr a
  = PrimC (CPrim a)
  | AddC (CPrim a) (CPrim a) a
  | SubC (CPrim a) (CPrim a) a
  | MulC (CPrim a) (CPrim a) a
  | DivC (CPrim a) (CPrim a) a
  | EqC (CPrim a) (CPrim a) a
  | LtC (CPrim a) (CPrim a) a
  | AppC (CPrim a) [CPrim a] a
  | TupC [CPrim a] a
  | SeqC (CExpr a) (CExpr a) a
  | IfC (CPrim a) (CExpr a) (CExpr a) a
  | LetC KVar (CExpr a) (CExpr a) a
  deriving (Show, Eq)

cprimTag :: CPrim a -> a
cprimTag (UnitC a) = a
cprimTag (IntC _ a) = a
cprimTag (VarC _ a) = a
cprimTag (ClsC _ _ a) = a

cexprTag :: CExpr a -> a
cexprTag (PrimC p) = cprimTag p
cexprTag (AddC _ _ a) = a
cexprTag (SubC _ _ a) = a
cexprTag (MulC _ _ a) = a
cexprTag (DivC _ _ a) = a
cexprTag (EqC _ _ a) = a
cexprTag (LtC _ _ a) = a
cexprTag (AppC _ _ a) = a
cexprTag (TupC _ a) = a
cexprTag (SeqC _ _ a) = a
cexprTag (IfC _ _ _ a) = a
cexprTag (LetC _ _ _ a) = a

type TypedCExpr = CExpr Type
type TypedCPrim = CPrim Type

data Program = Program
  { programClosures :: [ClsDef]
  , programMain :: TypedCExpr
  } deriving (Show, Eq)

data FunDef = FunDef
  { funName :: String
  , funArg :: String
  , funArgType :: Type
  , funBody :: TypedCExpr
  } deriving (Show, Eq)

data ClsDef = ClsDef
  { clsName :: Int
  , clsFreeVars :: [(String, Type)]
  , clsArg :: KVar
  , clsArgType :: Type
  , clsBody :: TypedCExpr
  , clsBodyType :: Type
  } deriving (Show, Eq)
