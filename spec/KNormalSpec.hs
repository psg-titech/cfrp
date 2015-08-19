module KNormalSpec (spec) where

import Test.Hspec

import Language.CFrp.Syntax
import Language.CFrp.KNormal

spec :: Spec
spec = describe "kNormalize" $ do
  it "works" $ do
    let a = VarT 1
    -- \f x y -> f y (y + x)
    let ke =
          AbsE "f"
            (AbsE "x"
              (AbsE "y"
                (AppE
                  (VarE "f" (FunT IntT (FunT IntT a)))
                  [VarE "y" IntT, AddE (VarE "y" IntT) (VarE "x" IntT) IntT]
                  a)
                (FunT IntT a))
              (FunT IntT (FunT IntT a)))
            (FunT (FunT IntT (FunT IntT a)) (FunT IntT (FunT IntT a)))
    case kNormalize ke of
      AbsK (KVarS "f")
        (AbsK (KVarS "x")
          (AbsK (KVarS "y")
            (LetK (KVarI 0)
              (AddK (VarK (KVarS "y") IntT) (VarK (KVarS "x") IntT) IntT)
              (AppK
                (VarK (KVarS "f") (FunT IntT (FunT IntT (VarT n1))))
                [ VarK (KVarS "y") IntT
                , VarK (KVarI 0) IntT
                ]
                (VarT n2))
              (VarT n3))
            (FunT IntT (VarT n4)))
          (FunT IntT (FunT IntT (VarT n5))))
        (FunT (FunT IntT (FunT IntT (VarT n6))) (FunT IntT (FunT IntT (VarT n7)))) -> do
        n1 `shouldSatisfy` (> 0)
        n2 `shouldBe` n1
        n3 `shouldBe` n1
        n4 `shouldBe` n1
        n5 `shouldBe` n1
        n6 `shouldBe` n1
        n7 `shouldBe` n1
      other -> error $ show other
