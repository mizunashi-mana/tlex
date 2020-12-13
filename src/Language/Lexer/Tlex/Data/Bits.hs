module Language.Lexer.Tlex.Data.Bits (
    maxBitSize,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.Bits                   as Bits


maxBitSize :: Bits.FiniteBits a => Ord a => Num a => a -> Int
maxBitSize n = go 1 2 where
    go i m
        | n < m                     = i
        | i >= Bits.finiteBitSize n = i
        | otherwise                 = go
            do i + 1
            do m * 2
