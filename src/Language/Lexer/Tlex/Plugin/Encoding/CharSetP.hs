module Language.Lexer.Tlex.Plugin.Encoding.CharSetP (
    Pattern,
    CharSetStdP,
    CharSetP (..),
    charSetP,
    charSetPWithWarnings,
    chP,
    charsP,
    stringP,

    EncodeWarning (..),
    CharSetEncoder (..),
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.CharSet                      as CharSet
import qualified Data.String                       as String
import qualified Data.Word                         as Word
import qualified Language.Lexer.Tlex.Data.Reporter as Reporter
import qualified Language.Lexer.Tlex.Syntax        as Tlex


type Pattern = Tlex.Pattern Word.Word8

type CharSetStdP = CharSetP Identity

newtype CharSetP m = CharSetP
    { charSetEncodingP :: CharSet.CharSet -> m Pattern
    }

data EncodeWarning
    = NotSupportedChar Char
    | CustomWarning String.String
    deriving (Eq, Show)

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


class Monad m => CharSetEncoder m where
    reportEncodeWarning :: EncodeWarning -> m ()

instance CharSetEncoder Identity where
    reportEncodeWarning _ = pure ()

instance CharSetEncoder (Either EncodeWarning) where
    reportEncodeWarning e = Left e

instance CharSetEncoder (Reporter.Reporter EncodeWarning) where
    reportEncodeWarning e = Reporter.report e
