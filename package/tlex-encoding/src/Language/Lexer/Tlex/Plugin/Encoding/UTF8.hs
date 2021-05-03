module Language.Lexer.Tlex.Plugin.Encoding.UTF8 (
    charSetPUtf8,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.CharSet                                   as CharSet
import qualified Data.EnumMap.Strict                            as EnumMap
import qualified Data.EnumSet                                   as EnumSet
import qualified Data.IntSet                                    as IntSet
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
            pure do complementPFromEnumStrings bsSet

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

complementPFromEnumStrings
    :: NonEmptyEnumStringSet.NonEmptyEnumStringSet Word8 -> Tlex.Pattern Word8
complementPFromEnumStrings ess0 = Tlex.orP
        [ go [pr1es] []
        , go [pr2es, seqes] [seqesP]
        , go [pr3p1o1es, pr3p1o2es, seqes]
            [ Tlex.straightEnumSetP pr3p1o1es
            , Tlex.straightEnumSetP pr3p1o2es
            , seqesP
            ]
        , go [pr3p2es, seqes, seqes]
            [ Tlex.straightEnumSetP pr3p2es
            , seqesP
            , seqesP
            ]
        , go [pr3p3o1es, pr3p3o2es, seqes]
            [ Tlex.straightEnumSetP pr3p3o1es
            , Tlex.straightEnumSetP pr3p3o2es
            , seqesP
            ]
        , go [pr3p4es, seqes, seqes]
            [ Tlex.straightEnumSetP pr3p4es
            , seqesP
            , seqesP
            ]
        , go [pr4p1o1es, pr4p1o2es, seqes, seqes]
            [ Tlex.straightEnumSetP pr4p1o1es
            , Tlex.straightEnumSetP pr4p1o2es
            , seqesP
            , seqesP
            ]
        , go [pr4p2es, seqes, seqes, seqes]
            [ Tlex.straightEnumSetP pr4p1o1es
            , seqesP
            , seqesP
            , seqesP
            ]
        , go [pr4p3o1es, pr4p3o2es, seqes, seqes]
            [ Tlex.straightEnumSetP pr4p3o1es
            , Tlex.straightEnumSetP pr4p3o2es
            , seqesP
            , seqesP
            ]
        ]
    where
        seqes = EnumSet.fromList @Word8 [0x80..0xBF]
        seqesP = Tlex.straightEnumSetP seqes

        pr1es = EnumSet.fromList @Word8 [0x00..0x7F]
        pr2es = EnumSet.fromList @Word8 [0xC2..0xDF]
        pr3p1o1es = EnumSet.fromList @Word8 [0xE0]
        pr3p1o2es = EnumSet.fromList @Word8 [0xA0..0xBF]
        pr3p2es = EnumSet.fromList @Word8 [0xE1..0xEC]
        pr3p3o1es = EnumSet.fromList @Word8 [0xED]
        pr3p3o2es = EnumSet.fromList @Word8 [0x80..0x9F]
        pr3p4es = EnumSet.fromList @Word8 [0xEE..0xEF]
        pr4p1o1es = EnumSet.fromList @Word8 [0xF0]
        pr4p1o2es = EnumSet.fromList @Word8 [0x90..0xBF]
        pr4p2es = EnumSet.fromList @Word8 [0xF1..0xF3]
        pr4p3o1es = EnumSet.fromList @Word8 [0xF4]
        pr4p3o2es = EnumSet.fromList @Word8 [0x80..0x8F]

        go bess restPs = go' bess restPs ess0

        go' bess restPs ess = case bess of
            []    -> mempty
            [bes] -> Tlex.straightEnumSetP
                do bes `EnumSet.difference` NonEmptyEnumStringSet.singleEnums ess
            bes:bess2 ->
                let mess = NonEmptyEnumStringSet.enumStrings ess
                    (nes, ces) = EnumSet.partition
                        do \be -> EnumMap.member be mess
                        bes
                    cesP = Tlex.straightEnumSetP ces <> mconcat restPs
                in Tlex.orP do
                    cesP:
                        [ go' bess2 nrestPs ness
                        | ne <- EnumSet.toList nes
                        , let ness = case EnumMap.lookup ne mess of
                                Just x  -> x
                                Nothing -> error "unreachable"
                        , let nrestPs = case restPs of
                                []   -> []
                                _:xs -> xs
                        ]
