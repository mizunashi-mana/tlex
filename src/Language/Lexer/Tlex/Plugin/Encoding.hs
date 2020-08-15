module Language.Lexer.Tlex.Plugin.Encoding where

import           Language.Lexer.Tlex.Prelude

import qualified Data.CharSet                as CharSet
import qualified Data.IntSet                 as IntSet
import qualified Data.String                 as String
import qualified Data.Word                   as Word
import qualified Data.Char                   as Char
import qualified Language.Lexer.Tlex.Syntax  as Tlex
import qualified Data.Bits                   as Bits


type Pattern = Tlex.Pattern Word.Word8

newtype CharSetP = CharSetP
    { charSetEncodingP :: CharSet.CharSet -> (Pattern, CharSet.CharSet)
    }

charSetP :: CharSetP -> CharSet.CharSet -> Pattern
charSetP p cs = let (r, _) = charSetEncodingP p cs in r


charSetPUtf8 :: CharSetP
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

        goStraight l = goStraight' CharSet.empty l

        goStraight' ex = \case
            [] -> (Tlex.Empty, ex)
            c:cs -> case byteOffset c of
                -1 -> goStraight'
                    do CharSet.insert
                        do Char.chr c
                        do ex
                    do cs
                i -> buildStraightP i [c] ex [] cs

        buildStraightP i l ex ps = \case
            [] -> (Tlex.orP do straightP l i:ps, ex)
            c:cs -> case byteOffset c of
                -1 -> buildStraightP i l
                    do CharSet.insert
                        do Char.chr c
                        do ex
                    do ps
                    do cs
                ni
                    | ni == i -> buildStraightP i (c:l) ex ps cs
                    | otherwise -> buildStraightP ni [c] ex
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


chP :: CharSetP -> Char -> Pattern
chP p c = charSetP p do CharSet.singleton c

charsP :: CharSetP -> [Char] -> Pattern
charsP p cs = charSetP p do CharSet.fromList cs

stringP :: CharSetP -> String.String -> Pattern
stringP p s = foldMap
    do chP p
    do s
