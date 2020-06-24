module Rover.RoverSpec (spec) where

import Test.Hspec
import Rover

spec :: Spec
spec = do
    describe "Rover" $ do
        it "does something" $ do
            0 `shouldBe` 0
