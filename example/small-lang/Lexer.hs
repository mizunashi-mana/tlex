{-# LANGUAGE TemplateHaskell #-}

module Lexer where

import qualified Data.CharSet                  as CharSet
import qualified Data.CharSet.Unicode          as UniCharSet
import qualified Language.Haskell.TH           as TH
import qualified Language.Lexer.Tlex           as Tlex
import qualified Language.Lexer.Tlex.Plugin.TH as TlexTH

type LexerState = ()
type LexerAction = ()

initialRule :: Tlex.Pattern -> TH.Q (TH.TExp LexerAction)
    -> TlexTH.THScannerBuilder LexerState LexerAction ()
initialRule = TlexTH.thLexRule [()]

buildLexer :: TH.Q [TH.Dec]
buildLexer = TlexTH.outputScanner
    $ TlexTH.buildTHScannerWithReify lexerRules

lexerRules :: TlexTH.THScannerBuilder LexerState () ()
lexerRules = do
    initialRule whitespaceP [||()||]
    initialRule reservedOpP [||()||]
    initialRule varidP [||()||]
    initialRule litIntegerP [||()||]

whitespaceP = Tlex.someP whitecharP
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

varidP = smallP <> Tlex.manyP (Tlex.orP [smallP, largeP, digitP])

litIntegerP = Tlex.someP digitP

smallP = charSetP $ CharSet.range 'a' 'z'
largeP = charSetP $ CharSet.range 'A' 'Z'
digitP = charSetP $ CharSet.range '0' '9'


charSetP :: CharSet.CharSet -> Tlex.Pattern
charSetP cs = Tlex.Range cs

chP :: Char -> Tlex.Pattern
chP c = Tlex.Range $ CharSet.singleton c

charsP :: [Char] -> Tlex.Pattern
charsP cs = charSetP $ CharSet.fromList cs

stringP :: String -> Tlex.Pattern
stringP s = foldMap chP s
