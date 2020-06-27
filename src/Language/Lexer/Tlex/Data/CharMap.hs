module Language.Lexer.Tlex.Data.CharMap (
    CharMap,
    IntMap.empty,
    insert,
) where

import qualified Data.Char as Char
import qualified Data.IntMap as IntMap


type Key = Char.Char

type CharMap = IntMap.IntMap

insert :: Key -> a -> CharMap a -> CharMap a
insert c x m = IntMap.insert (Char.ord c) x m
