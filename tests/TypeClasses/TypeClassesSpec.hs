module TypeClasses.TypeClassesSpec (spec) where

import Ch2.TypeClasses (allTurnsInUse, orientRotateAgree, randomDirections, randomTurns, rotationsMonoidAgree)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "RandomDirections" $ do
        it "orientRotateAgree" $ do
            ds <- randomDirections 1000
            orientRotateAgree ds `shouldBe` True

        it "allTurnsInUse" $
            allTurnsInUse `shouldBe` True

        it "rotationsMonoidAgree" $ do
            ts <- randomTurns 1000
            rotationsMonoidAgree ts `shouldBe` False