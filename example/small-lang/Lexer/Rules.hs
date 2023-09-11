{-# LANGUAGE TemplateHaskellQuotes #-}

module Lexer.Rules where

import qualified Data.CharSet                        as CharSet
import qualified Data.Word                           as Word
import qualified Language.Haskell.TH                 as TH
import qualified Language.Lexer.Tlex                 as Tlex
import qualified Language.Lexer.Tlex.Plugin.Encoding as TlexEnc
import qualified Language.Lexer.Tlex.Plugin.TH       as TlexTH


type LexerState = ()
type LexerAction = ()
type LexerCodeUnit = Word.Word8

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit

initialRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
initialRule = TlexTH.thLexRule [()]

buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    lexer <- TlexTH.buildTHScannerWithReify lexerRules
    TlexTH.outputScanner lexer

lexerRules :: ScannerBuilder ()
lexerRules = do
    initialRule (Tlex.someP whitecharP) [||()||]
    initialRule specialP [||()||]
    initialRule reservedOpP [||()||]
    initialRule varidP [||()||]
    initialRule litIntegerP [||()||]

specialP = charsP
    [ '('
    , ')'
    ]

whitecharP = charsP
    [ ' '
    , '\t'
    , '\n'
    , '\r'
    ]

reservedOpP = Tlex.orP
    [ chP '\\'
    , stringP "->"
    , chP '+'
    , chP '*'
    ]

varidP = smallP
    <> Tlex.manyP (Tlex.orP [smallP, largeP, digitP])

litIntegerP = Tlex.someP digitP

smallP = charSetP $ CharSet.range 'a' 'z'
largeP = charSetP $ CharSet.range 'A' 'Z'
digitP = charSetP $ CharSet.range '0' '9'

charSetP :: CharSet.CharSet -> Pattern
charSetP cs = TlexEnc.charSetP TlexEnc.charSetPUtf8 cs

chP :: Char -> Pattern
chP c = TlexEnc.chP TlexEnc.charSetPUtf8 c

charsP :: [Char] -> Pattern
charsP cs = TlexEnc.charsP TlexEnc.charSetPUtf8 cs

stringP :: String -> Pattern
stringP s = TlexEnc.stringP TlexEnc.charSetPUtf8 s
