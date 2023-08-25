module TypeClasses.TypeClassesSpec (spec, spec2) where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    let square x = x * x :: Int

    describe "strip" $ do
        it "Test one" $
            square 4 `shouldBe` 16

        it "Test two" $
            square 5 `shouldBe` 25

    describe "strip2" $ do
        it "Test 3" $
            square 10 `shouldBe` 100

spec2 :: Spec
spec2 = do
    let square x = x * x :: Int

    describe "strip" $ do
        it "Test one" $
            square 4 `shouldBe` 16

        it "Test two" $
            square 5 `shouldBe` 25

    describe "strip2" $ do
        it "Test 3" $
            square 10 `shouldBe` 100