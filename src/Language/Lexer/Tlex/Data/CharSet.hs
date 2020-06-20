module Language.Lexer.Tlex.Data.CharSet
  (
      CharSet,
  ) where

import Language.Lexer.Tlex.Prelude

import qualified Data.Range.DiscreteRangeSet as RangeSet


type CharSet = RangeSet.DiscreteRangeSet Char
