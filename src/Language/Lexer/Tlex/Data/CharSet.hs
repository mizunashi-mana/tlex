module Language.Lexer.Tlex.Data.CharSet (
    module Data.CharSet,
    toElements,
    CharSetElements (..),
) where

import Prelude

import Data.CharSet hiding (toList, toAscList)
import qualified Data.IntSet as IntSet

data CharSetElements
    = StraightChars [Char]
    | ComplementChars [Char]
    deriving (Eq, Show)

toElements :: CharSet -> CharSetElements
toElements cs = case fromCharSet cs of
    (True, is)  -> StraightChars [ toEnum i | i <- IntSet.toAscList is ]
    (False, is) -> ComplementChars [ toEnum i | i <- IntSet.toAscList is ]
