module Language.Lexer.Tlex.Plugin.Encoding (
    Pattern,
    CharSetStdP,
    CharSetP (..),
    charSetP,
    charSetPWithWarnings,
    chP,
    charsP,
    stringP,

    charSetPUtf8,

    EncodeWarning (..),
    CharSetEncoder (..),
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.CharSet                as CharSet
import qualified Data.IntSet                 as IntSet
import qualified Data.String                 as String
import qualified Data.Word                   as Word
import qualified Language.Lexer.Tlex.Syntax  as Tlex
import qualified Data.Bits                   as Bits
import qualified Language.Lexer.Tlex.Data.Reporter as Reporter


type Pattern = Tlex.Pattern Word.Word8

type CharSetStdP = CharSetP Identity

newtype CharSetP m = CharSetP
    { charSetEncodingP :: CharSet.CharSet -> m Pattern
    }

charSetP :: CharSetStdP -> CharSet.CharSet -> Pattern
charSetP p cs = runIdentity do charSetEncodingP p cs

charSetPWithWarnings :: CharSetEncoder m => CharSetP m -> CharSet.CharSet -> m Pattern
charSetPWithWarnings p cs = charSetEncodingP p cs

chP :: CharSetStdP -> Char -> Pattern
chP p c = charSetP p do CharSet.singleton c

charsP :: CharSetStdP -> [Char] -> Pattern
charsP p cs = charSetP p do CharSet.fromList cs

stringP :: CharSetStdP -> String.String -> Pattern
stringP p s = foldMap
    do chP p
    do s


charSetPUtf8 :: CharSetEncoder m => CharSetP m
charSetPUtf8 = CharSetP
    { charSetEncodingP = \case
        CharSet.CharSet True  _ is -> goStraight do IntSet.toAscList is
        CharSet.CharSet False _ is -> goComplement do IntSet.toAscList is
    }
    where
        byteOffset c = if
            | c <= 0x7F   -> 0x0
            | c <= 0x7FF  -> 0xC0 + Bits.shiftR c 6
            | c <= 0xD7FF -> 0xE0 + Bits.shiftR c 12
            | c <= 0xDFFF -> -1
            | c <= 0xFFFF -> 0xE0 + Bits.shiftR c 12
            | otherwise   -> 0xF0 + Bits.shiftR c 18

        goStraight = \case
            [] -> pure Tlex.Empty
            c:cs -> case byteOffset c of
                -1 -> do
                    reportNotSupportedWarning c
                    goStraight cs
                i  -> buildStraightP i [c] [] cs

        buildStraightP i l ps = \case
            [] -> pure do Tlex.orP do straightP l i:ps
            c:cs -> case byteOffset c of
                -1 -> do
                    reportNotSupportedWarning c
                    buildStraightP i l ps cs
                ni
                    | ni == i   -> buildStraightP i (c:l) ps cs
                    | otherwise -> buildStraightP ni [c]
                        do straightP l i:ps
                        do cs

        straightP l i = if
            | i == 0x0  -> Tlex.enumsP [fromIntegral c | c <- l]
            | i <= 0xDF -> Tlex.enumsP [fromIntegral i]
                <> Tlex.enumsP [fromIntegral do 0x80 + c Bits..&. 0xBF | c <- l]
            | i <= 0xEF -> Tlex.enumsP [fromIntegral i]
                <> undefined l
            | otherwise -> Tlex.enumsP [fromIntegral i]
                <> undefined l

        goComplement = undefined

        reportNotSupportedWarning c = reportEncodeWarning
            do NotSupportedChar do toEnum c


data EncodeWarning
    = NotSupportedChar Char
    | CustomWarning String.String
    deriving (Eq, Show)

class Monad m => CharSetEncoder m where
    reportEncodeWarning :: EncodeWarning -> m ()

instance CharSetEncoder Identity where
    reportEncodeWarning _ = pure ()

instance CharSetEncoder (Either EncodeWarning) where
    reportEncodeWarning e = Left e

instance CharSetEncoder (Reporter.Reporter EncodeWarning) where
    reportEncodeWarning e = Reporter.report e
