{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Range.DiscreteRangeSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Prelude

import Data.Range.DiscreteOrd
import Data.Range.DiscreteRange

spec :: Spec
spec = do
    describe "compareAndMerge" do
        it "is anti-symmetric" $ property \(x :: DiscreteRange Int) y ->
            compareAndMerge x y == flipOrderingMerged do compareAndMerge y x

    describe "compareAndFindCommon" do
        it "is anti-symmetric" $ property \(x :: DiscreteRange Int) y ->
            compareAndFindCommon x y == flipOrderingWithCommon do compareAndFindCommon y x

        it "is restricted compareAndMerge" $ property \(x :: DiscreteRange Int) y ->
            let resultWithCommon = compareAndFindCommon x y in
            case compareAndMerge x y of
                LessThanCannotMerge    -> resultWithCommon == LessThanWithoutCommon
                GreaterThanCannotMerge -> resultWithCommon == GreaterThanWithoutCommon
                Merged{}               -> True


instance (DiscreteOrd a, Arbitrary a) => Arbitrary (DiscreteRange a) where
    arbitrary = discreteRange <$> arbitrary <*> arbitrary
