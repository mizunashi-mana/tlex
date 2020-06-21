module Language.Lexer.Tlex.Data.CharSet (
    CharSet,
    module Data.Range.DiscreteRangeSet,
) where

import Language.Lexer.Tlex.Prelude

import Data.Range.DiscreteRangeSet


type CharSet = DiscreteRangeSet Char
