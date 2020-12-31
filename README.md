# Tlex: A Generator for Lexical Analysers

![Haskell CI](https://github.com/mizunashi-mana/tlex/workflows/Haskell%20CI/badge.svg)

## Installation

Add dependencies on `package.cabal`:

```
build-depends:
    base,
    bytestring,
    tlex,          -- main
    tlex-encoding, -- for utf8 parsing
    tlex-th,       -- for outputing lexer with Template Haskell
    charset,
    template-haskell,
```

## Usage

Setup:

```haskell
import qualified Data.CharSet                        as CharSet
import qualified Data.Word                           as Word
import qualified Language.Haskell.TH                 as TH
import qualified Language.Lexer.Tlex                 as Tlex
import qualified Language.Lexer.Tlex.Plugin.Encoding as TlexEnc
import qualified Language.Lexer.Tlex.Plugin.TH       as TlexTH


type LexerState = ()
type LexerAction = [LexerCodeUnit] -> Token
type LexerCodeUnit = Word.Word8

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit

rule :: Pattern -> TH.Q (TH.TExp LexerAction) -> ScannerBuilder ()
rule = TlexTH.thLexRule [()]
```

Setup `charSetP`:

```haskell
charSetP :: CharSet.CharSet -> Pattern
charSetP cs = TlexEnc.charSetP TlexEnc.charSetPUtf8 cs

chP :: Char -> Pattern
chP c = TlexEnc.chP TlexEnc.charSetPUtf8 c
```

Write lexer rules:

```haskell
buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    lexer <- TlexTH.buildTHScannerWithReify lexerRules
    TlexTH.outputScanner lexer

data Token
    = TokWhiteSpace [LexerCodeUnit]
    | TokSmallAlpha [LexerCodeUnit]
    | TokLargeAlpha [LexerCodeUnit]
    | TokDigit [LexerCodeUnit]

lexerRules :: ScannerBuilder ()
lexerRules = do
    rule (Tlex.someP whitecharP) [||TokWhiteSpace||]
    rule (charSetP $ CharSet.range 'a' 'z') [||TokSmallAlpha||]
    rule (charSetP $ CharSet.range 'A' 'Z') [||TokLargeAlpha||]
    rule (charSetP $ CharSet.range '0' '9') [||TokDigit||]

whitecharP = Tlex.orP
    [ chP ' '
    , '\t'
    , '\n'
    , '\r'
    ]
```

Build lexer:

```haskell
$(Lexer.Rules.buildLexer)

newtype InputByteString a = InputByteString
    { unInputByteString :: ByteString -> Int -> (a, Int)
    }
    deriving (Functor, Applicative, Monad)
        via (ReaderT ByteString (State Int))

runInputByteString :: InputByteString a -> ByteString -> (a, Int)
runInputByteString (InputByteString runner) input = runner input 0

instance TlexContext Int Word8 InputByteString where
    tlexGetInputPart = InputString $ \bs i -> (bs `indexMaybe` i, i)
    tlexGetMark = InputByteString $ \bs i -> (i, i)

lexByteString :: ByteString.ByteString -> Maybe [ByteString.ByteString]
lexByteString s0 = go s0 id where
    go s acc = case runInputByteString (tlexScan ()) s of
        (TlexEndOfInput, _)            -> Just $ acc []
        (TlexError, _)                 -> Nothing
        (TlexAccepted n act, _) ->
            let (consumed, rest) = splitAt n s
                token = act consumed
            in go rest $ \n -> acc act:n
```

## Examples

* Small language: https://github.com/mizunashi-mana/tlex/tree/master/example/small-lang
* Haskell2010: https://github.com/mizunashi-mana/tlex/tree/master/example/haskell2010
