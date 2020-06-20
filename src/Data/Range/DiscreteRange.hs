module Data.Range.DiscreteRange
    (
        DiscreteRange,
        discreteRange,
        elem,
        bounds,
        DiscreteRangeOrderingMerged (..),
        compareAndMerge,
        flipOrderingMerged,
        DiscreteRangeOrderingWithCommon (..),
        compareAndFindCommon,
        flipOrderingWithCommon,
    ) where

import Prelude hiding (elem)
import GHC.Generics (Generic)

import Data.Range.DiscreteOrd


data DiscreteRange a = DiscreteRange a a
    deriving (Eq, Show, Generic)

discreteRange :: DiscreteOrd a => a -> a -> DiscreteRange a
discreteRange x y
  | x < y     = DiscreteRange x y
  | otherwise = DiscreteRange y x

elem :: DiscreteOrd a => a -> DiscreteRange a -> Bool
elem x (DiscreteRange l u) = l <= x && x <= u

bounds :: DiscreteRange a -> (a, a)
bounds (DiscreteRange l u) = (l, u)


data DiscreteRangeOrderingMerged a
    = LessThanCannotMerge
    | GreaterThanCannotMerge
    | Merged (DiscreteRange a)
    deriving (Eq, Show, Generic)

flipOrderingMerged :: DiscreteOrd a => DiscreteRangeOrderingMerged a -> DiscreteRangeOrderingMerged a
flipOrderingMerged = \case
    LessThanCannotMerge    -> GreaterThanCannotMerge
    GreaterThanCannotMerge -> LessThanCannotMerge
    Merged r               -> Merged r

-- |
--
-- >>> compareAndMerge (discreteRange 0 1) (discreteRange 10 11)
-- LessThanCannotMerge
-- >>> compareAndMerge (discreteRange 10 11) (discreteRange 0 1)
-- GreaterThanCannotMerge
-- >>> compareAndMerge (discreteRange 0 3) (discreteRange 2 4)
-- Merged (DiscreteRange 0 4)
-- >>> compareAndMerge (discreteRange 0 1) (discreteRange 2 3)
-- Merged (DiscreteRange 0 3)
--
compareAndMerge :: DiscreteOrd a => DiscreteRange a -> DiscreteRange a -> DiscreteRangeOrderingMerged a
compareAndMerge (DiscreteRange l1 u1) (DiscreteRange l2 u2) = go
    where
        go = case neighboredCompare u1 l2 of
            NeighborEqual       -> Merged do DiscreteRange l1 u2
            NeighborLessThan    -> Merged do DiscreteRange l1 u2
            NeighborGreaterThan -> Merged do DiscreteRange (l1 `min` l2) (u1 `max` u2)
            FarLessThan         -> LessThanCannotMerge
            FarGreaterThan      -> go2

        go2 = case neighboredCompare u2 l1 of
            NeighborEqual       -> Merged do DiscreteRange l2 u1
            NeighborLessThan    -> Merged do DiscreteRange l2 u1
            NeighborGreaterThan -> Merged do DiscreteRange (l1 `min` l2) (u1 `max` u2)
            FarLessThan         -> GreaterThanCannotMerge
            FarGreaterThan      -> Merged do DiscreteRange (l1 `min` l2) (u1 `max` u2)


data DiscreteRangeOrderingWithCommon a
    = LessThanWithoutCommon
    | GreaterThanWithoutCommon
    | WithCommon (DiscreteRange a)
    deriving (Eq, Show, Generic)

flipOrderingWithCommon
    :: DiscreteOrd a
    => DiscreteRangeOrderingWithCommon a -> DiscreteRangeOrderingWithCommon a
flipOrderingWithCommon = \case
    LessThanWithoutCommon    -> GreaterThanWithoutCommon
    GreaterThanWithoutCommon -> LessThanWithoutCommon
    WithCommon r             -> WithCommon r

-- |
--
-- >>> compareAndFindCommon (discreteRange 0 1) (discreteRange 10 11)
-- LessThanWithCommon
-- >>> compareAndFindCommon (discreteRange 10 11) (discreteRange 0 1)
-- GreaterThanWithCommon
-- >>> compareAndFindCommon (discreteRange 0 3) (discreteRange 2 4)
-- WithCommon (DiscreteRange 2 3)
--
compareAndFindCommon :: DiscreteOrd a => DiscreteRange a -> DiscreteRange a -> DiscreteRangeOrderingWithCommon a
compareAndFindCommon (DiscreteRange l1 u1) (DiscreteRange l2 u2) = go
    where
        go = case u1 `compare` l2 of
            EQ -> WithCommon do DiscreteRange u1 l2
            LT -> LessThanWithoutCommon
            GT -> go2

        go2 = case u2 `compare` l1 of
            EQ -> WithCommon do DiscreteRange u2 l1
            LT -> GreaterThanWithoutCommon
            GT -> WithCommon do DiscreteRange (l1 `max` l2) (u1 `min` u2)
