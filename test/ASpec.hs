module ASpec where

import Test.Hspec
import Test.QuickCheck
import Exec
import Tree

spec :: Spec
spec = do
    describe "HAL tests" $ do
        describe "Define and invoc'" $ do
            it "Just Define : no return expected" $ do
                fst (executeTree (createTree "(define foo 21)") []) `shouldBe` ""
            it "Define and Use : foo=21 " $ do
                fst (executeTree (createTree "(* foo 2)") [Var {name = "foo", val = "21"}]) `shouldBe` "42"

        describe "Simple Maths Tests" $ do
            it "Addition : 21 + 2" $ do
                fst (executeTree (createTree "(+ foo 2)") [Var {name = "foo", val = "21"}]) `shouldBe` "23"
            it "Substraction : 21 - 2" $ do
                fst (executeTree (createTree "(- foo 2)") [Var {name = "foo", val = "21"}]) `shouldBe` "19"
            it "Multiplication : 21 * 2" $ do
                fst (executeTree (createTree "(* foo 2)") [Var {name = "foo", val = "21"}]) `shouldBe` "42"
            it "Division : 21 / 2" $ do
                fst (executeTree (createTree "(div foo 2)") [Var {name = "foo", val = "21"}]) `shouldBe` "10"
            it "Modulo : 21 % 2" $ do
                fst (executeTree (createTree "(mod foo 2)") [Var {name = "foo", val = "21"}]) `shouldBe` "1"
        
        describe "Simple Boolean Tests" $ do
            it "eq? : 21 /= 2" $ do
                fst (executeTree (createTree "(eq? foo 2)") [Var {name = "foo", val = "21"}]) `shouldBe` "#f"
            it "eq? : 21 = 21" $ do
                fst (executeTree (createTree "(eq? foo 21)") [Var {name = "foo", val = "21"}]) `shouldBe` "#t"
            it "< : 21 < 2" $ do
                fst (executeTree (createTree "(< foo 2)") [Var {name = "foo", val = "21"}]) `shouldBe` "#f"
            it "< : 21 < 22" $ do
                fst (executeTree (createTree "(< foo 22)") [Var {name = "foo", val = "21"}]) `shouldBe` "#t"

        describe "Simple Fonctions Tests" $ do
            it "cons : (cons 1 2)" $ do
                fst (executeTree (createTree "(cons 1 2)") []) `shouldBe` "(1 . 2)"
            it "cdr : (cdr (cons 1 2))" $ do
                fst (executeTree (createTree "(cdr (cons 1 2))") []) `shouldBe` "2"
            it "car : (car (cons 1 2))" $ do
                fst (executeTree (createTree "(car (cons 1 2))") []) `shouldBe` "1"
            it "quote - Print hello" $ do
                fst (executeTree (createTree "(quote hello)") []) `shouldBe` "hello"
            it "quote - Print (+ 1 2)" $ do
                fst (executeTree (createTree "(quote (+ 1 2))") []) `shouldBe` "(+ 1 2)"
            it "'(quote) - Print hello" $ do
                fst (executeTree (createTree "('hello)") []) `shouldBe` "hello"
            it "'(quote) - Print (+ 1 2)" $ do
                fst (executeTree (createTree "('(+ 1 2))") []) `shouldBe` "(+ 1 2)"
            it "atom : Expected True" $ do
                fst (executeTree (createTree "(atom? (+ 1 2))") []) `shouldBe` "#f"
            it "atom : Expected False" $ do
                fst (executeTree (createTree "(atom? bar)") []) `shouldBe` "#t"
            it "cond : Does it work" $ do
                fst (executeTree (createTree "(cond (#f 1) (#t (+ 1 1)))") []) `shouldBe` "2"

