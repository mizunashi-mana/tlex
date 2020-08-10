module Language.Lexer.Tlex.Plugin.Unicode where

import Language.Lexer.Tlex.Prelude

import qualified Data.CharSet                  as CharSet
import qualified Data.IntSet                   as IntSet
import qualified Data.Word                     as Word
import qualified Data.String                   as String
import qualified Language.Lexer.Tlex.Syntax    as Tlex


type Pattern = Tlex.Pattern Word.Word8

newtype CharSetP = CharSetP
    { charSetP :: CharSet.CharSet -> Pattern
    }


charSetPUtf8 :: CharSetP
charSetPUtf8 = CharSetP
    { charSetP = go
    }
    where
        go = \case
            CharSet.CharSet True  _ is -> goStraight do IntSet.toAscList is
            CharSet.CharSet False _ is -> goComplement do IntSet.toAscList is

        goStraight = undefined

        goComplement = undefined


chP :: CharSetP -> Char -> Pattern
chP p c = charSetP p do CharSet.singleton c

charsP :: CharSetP -> [Char] -> Pattern
charsP p cs = charSetP p do CharSet.fromList cs

stringP :: CharSetP -> String.String -> Pattern
stringP p s = foldMap
    do chP p
    do s
