module TypeInfSpec (spec) where

import qualified Data.Map as M
import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.Hspec.QuickCheck

import Language.CFrp.Syntax
import Language.CFrp.TypeInf

spec :: Spec
spec = describe "typeInf" $ do
  describe "int literal" $ do
    it "infers" $ do
      IntE 1 () `shouldBeTyped` IntE 1 IntT

  describe "unit literal" $ do
    it "infers" $ do
      UnitE () `shouldBeTyped` UnitE UnitT

  describe "tuple literal" $ do
    it "infers" $ do
      TupE
        [ UnitE ()
        , TupE [IntE 1 (), UnitE ()] ()
        , IntE 2 ()
        ]
        ()
      `shouldBeTyped`
      TupE
        [ UnitE UnitT
        , TupE [IntE 1 IntT, UnitE UnitT] (TupT [IntT, UnitT])
        , IntE 2 IntT
        ]
        (TupT [UnitT, TupT [IntT, UnitT], IntT])


  describe "+" $ do
    it "infers" $ do
      AddE (IntE 1 ()) (IntE 2 ()) () `shouldBeTyped` AddE (IntE 1 IntT) (IntE 2 IntT) IntT

    context "with non-integer type" $ do
      it "throws an error" $ do
        let ete = typeInf M.empty $ AddE (IntE 1 ()) (UnitE ()) ()
        ete `shouldSatisfy` isLeft
        case ete of
          Left (UnificationError _ _) -> return ()
          other -> error $ show other

  describe "abstraction" $ do
    it "infers simple type" $ do
      -- \x y -> x + y
      AbsE "x"
        (AbsE "y"
          (AddE (VarE "y" ()) (VarE "x" ()) ())
          ())
        ()
      `shouldBeTyped`
      AbsE "x"
        (AbsE "y"
          (AddE (VarE "y" IntT) (VarE "x" IntT) IntT)
          (FunT IntT IntT))
        (FunT IntT (FunT IntT IntT))

    it "doesn't generalize arguments" $ do
      -- \f -> (f 1, f ())
      let ete = typeInf M.empty $
            AbsE "f"
              (TupE
                [ AppE (VarE "f" ()) [IntE 1 ()] ()
                , AppE (VarE "f" ()) [UnitE ()] ()
                ]
                ())
              ()
      ete `shouldSatisfy` isLeft
      case ete of
        Left (UnificationError _ _) -> return ()
        other -> error $ show other

  describe "application" $ do
    it "infers polymorphic type" $ do
      -- \f x y -> f y (y + x)
      let pe =
            AbsE "f"
              (AbsE "x"
                (AbsE "y"
                  (AppE
                    (VarE "f" ())
                    [VarE "y" (), AddE (VarE "y" ()) (VarE "x" ()) ()]
                    ())
                  ())
                ())
              ()
      case typeInf M.empty pe of
        Right
          (AbsE "f"
            (AbsE "x"
              (AbsE "y"
                (AppE
                  (VarE "f" (FunT IntT (FunT IntT a1)))
                  [VarE "y" IntT, AddE (VarE "y" IntT) (VarE "x" IntT) IntT]
                  a2)
                (FunT IntT a3))
              (FunT IntT (FunT IntT a4)))
            (FunT (FunT IntT (FunT IntT a5)) (FunT IntT (FunT IntT a6))))
          -> do
            a2 `shouldBe` a1
            a3 `shouldBe` a1
            a4 `shouldBe` a1
            a5 `shouldBe` a1
            a6 `shouldBe` a1
        te -> error $ show te

  describe "let" $ do
    it "infers simple type" $ do
      -- \x -> let y = x + 1 in y
      AbsE "x"
        (LetE "y"
          (AddE (VarE "x" ()) (IntE 1 ()) ())
          (VarE "y" ())
          ())
        ()
      `shouldBeTyped`
      AbsE "x"
        (LetE "y"
          (AddE (VarE "x" IntT) (IntE 1 IntT) IntT)
          (VarE "y" IntT)
          IntT)
        (FunT IntT IntT)

    it "infers polymorphic type" $ do
      -- let f = \x -> x in (f 1, f ())
      let pe =
            (LetE "f"
              (AbsE "x"
                (VarE "x" ())
                ())
              (AppE (VarE "f" ()) [IntE 1 ()] ())
              ())
      case typeInf M.empty pe of
        Right
          (LetE "f"
            (AbsE "x"
              (VarE "x" (VarT n1))
              (FunT (VarT n2) (VarT n3)))
            (AppE (VarE "f" (FunT IntT IntT)) [IntE 1 IntT] IntT)
            IntT) -> do
          n1 `shouldSatisfy` (> 0)
          n2 `shouldBe` n1
          n3 `shouldBe` n1
        te -> error $ show te

    it "correctly infers monomorphic type" $ do
      -- \x -> let y = x in (y 1, y ())
      let ete = typeInf M.empty $
            AbsE "x"
              (LetE "y"
                (VarE "x" ())
                (TupE
                  [ AppE (VarE "y" ()) [IntE 1 ()] ()
                  , AppE (VarE "y" ()) [UnitE ()] ()
                  ]
                  ())
                ())
              ()
      ete `shouldSatisfy` isLeft
      case ete of
        Left (UnificationError _ _) -> return ()
        other -> error $ show other

  it "occursCheck" $ do
    let ete = typeInf M.empty $
          AbsE "x"
            (AppE (VarE "x" ()) [VarE "x" ()] ())
            ()
    ete `shouldSatisfy` isLeft
    case ete of
      Left OccursCheckError -> return ()
      other -> error $ show other

  it "deals with VarT-VarT case" $ do
    -- \Lc -> (\K -> Lc) (Lc, Lc, ())
    let ete = typeInf M.empty $ AbsE "Lc" (AppE (AbsE "K" (VarE "Lc" ()) ()) [TupE [VarE "Lc" (),VarE "Lc" (),UnitE ()] ()] ()) ()
    ete `shouldSatisfy` isRight
    case ete of
      Right
        (AbsE "Lc"
          (AppE
            (AbsE "K"
              (VarE "Lc" (VarT n1))
              (FunT (TupT [VarT n2, VarT n3, UnitT]) (VarT n4)))
            [TupE
              [ VarE "Lc" (VarT n5)
              , VarE "Lc" (VarT n6)
              , UnitE UnitT
              ]
              (TupT [VarT n7, VarT n8, UnitT])]
            (VarT n9))
          (FunT (VarT n10) (VarT n11)))
        -> do
          n1 `shouldSatisfy` (> 0)
          n2 `shouldBe` n1
          n3 `shouldBe` n1
          n4 `shouldBe` n1
          n5 `shouldBe` n1
          n6 `shouldBe` n1
          n7 `shouldBe` n1
          n8 `shouldBe` n1
          n9 `shouldBe` n1
          n10 `shouldBe` n1
          n11 `shouldBe` n1
      other -> error $ show other

  prop "never returns negavie VarT" neverReturnsNegativeTypeVariable

shouldBeTyped :: ParsedExpr -> TypedExpr -> Expectation
pe `shouldBeTyped` te = do
  let ete = typeInf M.empty pe
  ete `shouldSatisfy` isRight
  let Right got = ete
  got `shouldBe` te

neverReturnsNegativeTypeVariable :: ParsedExpr -> Bool
neverReturnsNegativeTypeVariable expr =
  case typeInf M.empty expr of
    Right te -> checkE te
    Left _ -> True
  where
    checkE :: TypedExpr -> Bool
    checkE (AddE e1 e2 t) = checkT t && checkE e1 && checkE e2
    checkE (SubE e1 e2 t) = checkT t && checkE e1 && checkE e2
    checkE (MulE e1 e2 t) = checkT t && checkE e1 && checkE e2
    checkE (DivE e1 e2 t) = checkT t && checkE e1 && checkE e2
    checkE (EqE e1 e2 t) = checkT t && checkE e1 && checkE e2
    checkE (LtE e1 e2 t) = checkT t && checkE e1 && checkE e2
    checkE (IntE _ t) = checkT t
    checkE (VarE _ t) = checkT t
    checkE (AbsE _ e t) = checkT t && checkE e
    checkE (AppE e1 es t) = checkT t && checkE e1 && all checkE es
    checkE (TupE es t) = checkT t && all checkE es
    checkE (UnitE t) = checkT t
    checkE (IfE e1 e2 e3 t) = checkT t && checkE e1 && checkE e2 && checkE e3
    checkE (LetE _ e1 e2 t) = checkT t && checkE e1 && checkE e2
    checkE (SeqE e1 e2 t) = checkT t && checkE e1 && checkE e2

    checkT :: Type -> Bool
    checkT (VarT n) = n > 0
    checkT (FunT t1 t2) = checkT t1 && checkT t2
    checkT (SigT t) = checkT t
    checkT (TupT ts) = all checkT ts
    checkT UnitT = True
    checkT IntT = True
    checkT BoolT = True
