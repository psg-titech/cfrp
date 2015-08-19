module ParserSpec (spec) where
import Test.Hspec
import Test.Hspec.Expectations.Contrib

import Language.CFrp.Parser
import Language.CFrp.Syntax

spec :: Spec
spec = do
  describe "parseExprString" $ do
    it "parses arithmetic" $ do
      "(1 + x * 3) - _a0 / 5"
        `shouldBeParsedE`
        SubE
          (AddE
            (IntE 1 ())
            (MulE (VarE "x" ()) (IntE 3 ()) ())
            ())
          (DivE (VarE "_a0" ()) (IntE 5 ()) ())
          ()

    it "parses lambda" $ do
      "\\x -> \\_y10 -> 1 + _y10"
        `shouldBeParsedE`
        AbsE "x"
          (AbsE "_y10"
            (AddE (IntE 1 ()) (VarE "_y10" ()) ())
            ())
          ()

    it "parses application" $ do
      "4 * (\\x y -> x + y + 1) 2 3 / 5"
        `shouldBeParsedE`
        DivE
          (MulE
            (IntE 4 ())
            (AppE
              (AbsE "x"
                (AbsE "y"
                  (AddE
                    (AddE (VarE "x" ()) (VarE "y" ()) ())
                    (IntE 1 ())
                    ())
                  ())
                ())
              [IntE 2 (), IntE 3 ()]
              ())
            ())
          (IntE 5 ())
          ()

    it "parses tuple" $ do
      "(1, (x), (\\y -> y, (), 2))"
        `shouldBeParsedE`
        TupE [
            IntE 1 ()
          , VarE "x" ()
          , TupE [AbsE "y" (VarE "y" ()) (), UnitE (), IntE 2 ()] ()
          ]
          ()

    it "parses if" $ do
      "if \\x -> x then \\y -> y + 2 else \\z -> z + 3"
        `shouldBeParsedE`
        IfE (AbsE "x" (VarE "x" ()) ())
          (AbsE "y" (AddE (VarE "y" ()) (IntE 2 ()) ()) ())
          (AbsE "z" (AddE (VarE "z" ()) (IntE 3 ()) ()) ())
          ()

    it "parses let" $ do
      "let f x y = x + y in f 1 2"
        `shouldBeParsedE`
        LetE "f"
          (AbsE "x" (AbsE "y" (AddE (VarE "x" ()) (VarE "y" ()) ()) ()) ())
          (AppE (VarE "f" ()) [IntE 1 (), IntE 2 ()] ())
          ()

    it "parses sequence" $ do
      "let f n = print_int n; print_int n in f 10; f 20"
      `shouldBeParsedE`
      LetE "f"
        (AbsE "n"
          (SeqE
            (AppE (VarE "print_int" ()) [VarE "n" ()] ())
            (AppE (VarE "print_int" ()) [VarE "n" ()] ())
            ())
          ())
        (SeqE
          (AppE (VarE "f" ()) [IntE 10 ()] ())
          (AppE (VarE "f" ()) [IntE 20 ()] ())
          ())
        ()

  describe "parseDeclString" $ do
    it "parses input declaration" $ do
      "%input lastPress :: Signal Int = last_press_input_node;"
        `shouldBeParsedD`
        InputD "lastPress" (SigT IntT) "last_press_input_node" ()


    it "parses embed declaration" $ do
      let code = "int func(int x)\n{\n  if (x) { x += 1; };\nreturn x;\n}"
      ("%{\n" ++ code ++ "\n%}")
        `shouldBeParsedD`
        EmbedD code ()

  describe "parseProgramString" $ do
    it "parses program" $ do
      let code = "int func(int x)\n{\n  if (x) { x += 1; };\nreturn x;\n}"
      let prog = unlines
            [ "%input lastPress :: Signal Int = last_press_input_node;"
            , "%{\n" ++ code ++ "\n%}"
            , "f lastPress"
            ]
      putStrLn prog
      prog
        `shouldBeParsedP`
        ([ InputD "lastPress" (SigT IntT) "last_press_input_node" ()
         , EmbedD code ()
         ]
        , AppE (VarE "f" ()) [VarE "lastPress" ()] ()
        )

shouldBeParsedE :: String -> ParsedExpr -> Expectation
shouldBeParsedE = parsed parseExprString

shouldBeParsedD :: String -> ParsedDecl -> Expectation
shouldBeParsedD = parsed parseDeclString

shouldBeParsedP :: String -> ([ParsedDecl], ParsedExpr) -> Expectation
shouldBeParsedP = parsed parseProgramString

parsed :: (Show e, Show a, Eq a) => (String -> String -> Either e a) -> String -> a -> Expectation
parsed parser str expected = do
  let got = parser "<test>" str
  got `shouldSatisfy` isRight
  let Right e = got
  e `shouldBe` expected
