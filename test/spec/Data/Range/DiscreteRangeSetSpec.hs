{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Range.DiscreteRangeSetSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Prelude

import Data.Range.DiscreteOrd
import Data.Range.DiscreteRange
import Data.Range.DiscreteRangeSet


spec :: Spec
spec = do
    describe "union" do
        it "is correct any two elements" $ property \(x :: DiscreteRange Int) y ->
            rangeSet [x, y] == rangeSet [x] `union` rangeSet [y]

        it "is correct De Morgan's law" $ property \(x :: DiscreteRange Int) y ->
            rangeSet [x] `intersect` rangeSet [y] == complement (complement (rangeSet [x]) `union` complement (rangeSet [y]))

instance (DiscreteOrd a, Arbitrary a) => Arbitrary (DiscreteRange a) where
    arbitrary = discreteRange <$> arbitrary <*> arbitrary
