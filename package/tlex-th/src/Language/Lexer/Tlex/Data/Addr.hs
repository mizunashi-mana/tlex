module Language.Lexer.Tlex.Data.Addr (
    addrCodeUnitsLE,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.Bits                   as Bits


addrCodeUnitsLE :: Bits.Bits a => Integral a => Int -> a -> [Word8]
addrCodeUnitsLE us n
    | n >= 0    = take us
        do map
            do \m -> fromInteger do fromIntegral do mod8bit m
            do iterate (`Bits.shiftR` 8) n
    | n == -1   = replicate us 0xFF
    | otherwise = error "unsupported"
    where
        mod8bit = case Bits.bitSizeMaybe n of
            Nothing -> \x -> x `mod` 256
            Just bs
                | bs <= 8   -> \x -> x
                | otherwise -> \x -> x `mod` 256
