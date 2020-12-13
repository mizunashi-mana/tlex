module Language.Lexer.Tlex.Data.Bits (
    maxBitSize,
) where

import Language.Lexer.Tlex.Prelude


maxBitSize :: Ord a => Num a => a -> Int
maxBitSize n = go 1 2
    where
        go i m
            | n < m     = i
            | otherwise = go
                do i + 1
                do m * 2
