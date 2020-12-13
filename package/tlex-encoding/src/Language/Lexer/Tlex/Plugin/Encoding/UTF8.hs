module Language.Lexer.Tlex.Plugin.Encoding.UTF8 (
    charSetPUtf8,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.CharSet                                   as CharSet
import qualified Data.IntSet                                    as IntSet
import qualified Language.Lexer.Tlex.Data.EnumMap               as EnumMap
import qualified Language.Lexer.Tlex.Data.EnumSet               as EnumSet
import qualified Language.Lexer.Tlex.Data.NonEmptyEnumStringSet as NonEmptyEnumStringSet
import qualified Language.Lexer.Tlex.Plugin.Encoding.CharSetP   as CharSetP
import qualified Language.Lexer.Tlex.Syntax                     as Tlex


charSetPUtf8 :: CharSetP.CharSetEncoder m => CharSetP.CharSetP m
charSetPUtf8 = CharSetP.CharSetP
        { CharSetP.charSetEncodingP = \case
            CharSet.CharSet True  _ is -> goStraight is
            CharSet.CharSet False _ is -> goComplement is
        }
    where
        goStraight is = do
            bsSet <- charSetToByteStringSetUtf8 is
            pure do straightP bsSet

        straightP s =
            let singleByteP = Tlex.straightEnumSetP do
                    NonEmptyEnumStringSet.singleEnums s
            in Tlex.orP do
                singleByteP:
                    [ Tlex.enumsP [c] <> straightP s'
                    | (c, s') <- EnumMap.assocs do
                        NonEmptyEnumStringSet.enumStrings s
                    ]

        goComplement is = do
            bsSet <- charSetToByteStringSetUtf8 is
            pure do complementP bsSet

        complementP s =
            let m = NonEmptyEnumStringSet.enumStrings s
                ks = EnumSet.fromList do EnumMap.keys m
            in Tlex.orP
                [ complementP1 s
                , complementP2 m ks
                , complementP3 m ks
                , complementP4 m ks
                ]

        complementP1 s =
            let ks = EnumSet.fromList [0x00..0x7F]
                    `EnumSet.difference` NonEmptyEnumStringSet.singleEnums s
            in Tlex.straightEnumSetP ks

        complementP2 m ks =
            let ks' = EnumSet.fromList [0xC2..0xDF]
                    `EnumSet.intersection` ks
                rks = EnumSet.fromList [0xC2..0xDF]
                    `EnumSet.difference` ks'
                rksP = Tlex.straightEnumSetP rks
                    <> Tlex.enumsP [0x80..0xBF]
            in Tlex.orP do
                rksP : do
                    k <- EnumSet.toList ks'
                    sk <- toList do EnumMap.lookup k m
                    pure do
                        Tlex.straightEnumSetP do
                            EnumSet.fromList [0x80..0xBF] `EnumSet.difference`
                                NonEmptyEnumStringSet.singleEnums sk

        complementP3 _ ks =
            let ks' = EnumSet.fromList [0xE1..0xEF]
                    `EnumSet.intersection` ks
                rks = EnumSet.fromList [0xE1..0xEF]
                    `EnumSet.difference` ks'
                rksP = Tlex.straightEnumSetP rks
                    <> Tlex.enumsP [0x80..0xBF]
                    <> Tlex.enumsP [0x80..0xBF]
            in Tlex.orP do
                rksP : [] -- undefined m

        complementP4 _ _ = mempty -- undefined

charSetToByteStringSetUtf8 :: CharSetP.CharSetEncoder m
    => IntSet.IntSet -> m (NonEmptyEnumStringSet.NonEmptyEnumStringSet Word8)
charSetToByteStringSetUtf8 is = foldM
        do \s c -> foldStep s c
        do NonEmptyEnumStringSet.empty
        do IntSet.toAscList is
    where
        foldStep s c = if
            | c <= 0x7F -> pure do
                NonEmptyEnumStringSet.insertSingleByte
                    do fromIntegral c
                    do s
            | c <= 0x7FF ->
                let (c', l) = stringTails c 1
                in pure do
                    NonEmptyEnumStringSet.insert
                        do (0xC0 + c') :| l
                        do s
            | 0xD800 <= c && c <= 0xDFFF -> do
                CharSetP.reportEncodeWarning
                    do CharSetP.NotSupportedChar do toEnum c
                pure s
            | c <= 0xFFFF ->
                let (c', l) = stringTails c 2
                in pure do
                    NonEmptyEnumStringSet.insert
                        do (0xE0 + c') :| l
                        do s
            | otherwise ->
                let (c', l) = stringTails c 3
                in pure do
                    NonEmptyEnumStringSet.insert
                        do (0xF0 + c') :| l
                        do s

        stringTails :: Int -> Int -> (Word8, [Word8])
        stringTails c n = stringTails' c [] n
        stringTails' c l = \case
            0 -> (fromIntegral c, l)
            n ->
                let (c', x) = quotRem c 0x40
                    x' = fromIntegral do 0x80 + x
                in stringTails' c'
                    do x' : l
                    do n - 1
