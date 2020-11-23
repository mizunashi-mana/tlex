{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}

module Lexer where

import qualified Data.CharSet                  as CharSet
import qualified Data.CharSet.Unicode          as UniCharSet
import qualified Language.Haskell.TH           as TH
import qualified Language.Lexer.Tlex           as Tlex
import qualified Language.Lexer.Tlex.Plugin.TH as TlexTH
import qualified Language.Lexer.Tlex.Plugin.Encoding as TlexEnc
import qualified Data.Word as Word

data LexerState
    = Initial
    | NestedComment
    deriving (Eq, Show, Enum)

type LexerAction = ()
type CodeUnit = Word.Word8

type ScannerBuilder = TlexTH.THScannerBuilder LexerState CodeUnit LexerAction
type Pattern = Tlex.Pattern CodeUnit

initialRule :: Pattern -> TH.Q (TH.TExp LexerAction) -> ScannerBuilder ()
initialRule = TlexTH.thLexRule [Initial]

nestedCommentRule :: Pattern -> TH.Q (TH.TExp LexerAction) -> ScannerBuilder ()
nestedCommentRule = TlexTH.thLexRule [NestedComment]

buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    lexer <- TlexTH.buildTHScannerWithReify lexerRules
    TlexTH.outputScanner lexer

lexerRules :: ScannerBuilder ()
lexerRules = do
    initialRule (Tlex.someP whitecharP) [||()||]

    initialRule commentP [||()||]

    initialRule openComP [||()||]
    -- closeComP should be the head on nested comment mode to avoid conflicting.
    nestedCommentRule closeComP [||()||]
    nestedCommentRule anyWithNewlineP [||()||]

    initialRule specialP [||()||]

    -- reservedIdP should be before qvarid to avoid conflicting.
    initialRule reservedIdP [||()||]
    -- reservedOpP should be before qvarsym / qconsym to avoid conflicting.
    initialRule reservedOpP [||()||]

    initialRule qvaridP [||()||]
    initialRule qconidP [||()||]
    initialRule qvarsymP [||()||]
    initialRule qconsymP [||()||]

    initialRule litIntegerP [||()||]
    initialRule litFloatP [||()||]
    initialRule litCharP [||()||]
    initialRule litStringP [||()||]

-- See https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-160002.2

specialP = charSetP specialCs
specialCs = CharSet.fromList
    ['(', ')', ',', ';', '[', ']', '`', '{', '}']

whitecharP = Tlex.orP
    [ newlineP
    , vertabP
    , spaceP
    , tabP
    , uniWhiteP
    ]
newlineP = Tlex.orP
    [ returnP <> lineFeedP
    , returnP
    , lineFeedP
    , formFeedP
    ]
returnP = charSetP returnCs
returnCs = CharSet.singleton '\r'
lineFeedP = charSetP lineFeedCs
lineFeedCs = CharSet.singleton '\n'
formFeedP = charSetP formFeedCs
formFeedCs = CharSet.singleton '\f'
vertabP = charSetP vertabCs
vertabCs = CharSet.singleton '\v'
spaceP = charSetP spaceCs
spaceCs = CharSet.singleton ' '
tabP = charSetP tabCs
tabCs = CharSet.singleton '\t'
uniWhiteP = charSetP UniCharSet.space

commentP = dashesP <> Tlex.maybeP (anyWithoutSymbolP <> Tlex.manyP anyP) <> newlineP where
    anyWithoutSymbolP = charSetP $ anyCs `CharSet.difference` symbolCs
dashesP = dashP <> dashP <> Tlex.manyP dashP
dashP = chP '-'
openComP = stringP "{-"
closeComP = stringP "-}"

anyWithNewlineP = Tlex.orP
    [ graphicP
    , whitecharP
    ]
anyP = charSetP anyCs
anyCs = mconcat
    [ graphicCs
    , spaceCs
    , tabCs
    ]
graphicP = charSetP graphicCs
graphicCs = mconcat
    [ smallCs
    , largeCs
    , symbolCs
    , digitCs
    , specialCs
    , CharSet.singleton '"'
    , CharSet.singleton '\''
    ]

smallP = charSetP smallCs
smallCs = mconcat
    [ ascSmallCs
    , uniSmallCs
    , CharSet.singleton '_'
    ]
ascSmallCs = CharSet.range 'a' 'z'
uniSmallCs = UniCharSet.lowercaseLetter

largeP = charSetP largeCs
largeCs = mconcat
    [ ascLargeCs
    , uniLargeCs
    ]
ascLargeP = charSetP ascLargeCs
ascLargeCs = CharSet.range 'A' 'Z'
uniLargeCs = mconcat
    [ UniCharSet.uppercaseLetter
    , UniCharSet.titlecaseLetter
    ]

symbolP = charSetP symbolCs
symbolCs = mconcat
    [ ascSymbolCs
    , uniSymbolCs
    ]
    `CharSet.difference` mconcat
        [ specialCs
        , CharSet.singleton '_'
        , CharSet.singleton '"'
        , CharSet.singleton '\''
        ]
ascSymbolCs = CharSet.fromList
    [ '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>'
    , '?', '@', '\\', '^', '|', '-', '~', ':'
    ]
uniSymbolCs = mconcat
    [ UniCharSet.symbol
    , UniCharSet.punctuation
    ]
digitP = charSetP digitCs
digitCs = mconcat
    [ ascDigitCs
    , uniDigitCs
    ]
ascDigitCs = CharSet.range '0' '9'
uniDigitCs = UniCharSet.decimalNumber

octitP = charSetP octitCs
octitCs = CharSet.range '0' '7'
hexitP = charSetP hexitCs
hexitCs = mconcat
    [ digitCs
    , CharSet.range 'A' 'F'
    , CharSet.range 'a' 'f'
    ]

varidP = smallP <> Tlex.manyP (Tlex.orP [smallP, largeP, digitP, chP '\''])
conidP = largeP <> Tlex.manyP (Tlex.orP [smallP, largeP, digitP, chP '\''])
reservedIdP = Tlex.orP
    [ stringP "case"
    , stringP "class"
    , stringP "data"
    , stringP "default"
    , stringP "deriving"
    , stringP "do"
    , stringP "else"
    , stringP "foreign"
    , stringP "if"
    , stringP "import"
    , stringP "in"
    , stringP "infix"
    , stringP "infixl"
    , stringP "infixr"
    , stringP "instance"
    , stringP "let"
    , stringP "module"
    , stringP "newtype"
    , stringP "of"
    , stringP "then"
    , stringP "type"
    , stringP "where"
    , chP '_'
    ]

varsymP = symbolWithoutColonP <> Tlex.manyP symbolP where
    symbolWithoutColonP = charSetP (symbolCs `CharSet.difference` CharSet.singleton ':')
consymP = chP ':' <> Tlex.manyP symbolP
reservedOpP = Tlex.orP
    [ stringP ".."
    , chP ':'
    , stringP "::"
    , chP '='
    , chP '\\'
    , stringP "<-"
    , stringP "->"
    , chP '@'
    , chP '~'
    , stringP "=>"
    ]

modidP = Tlex.manyP (conidP <> chP '.') <> conidP

qvaridP = Tlex.maybeP (modidP <> chP '.') <> varidP
qconidP = Tlex.maybeP (modidP <> chP '.') <> conidP
qvarsymP = Tlex.maybeP (modidP <> chP '.') <> varsymP
qconsymP = Tlex.maybeP (modidP <> chP '.') <> consymP

decimalP = Tlex.someP digitP
octalP = Tlex.someP octitP
hexadecimalP = Tlex.someP hexitP

litIntegerP = Tlex.orP
    [ decimalP
    , stringP "0o" <> octalP
    , stringP "0O" <> octalP
    , stringP "0x" <> hexitP
    , stringP "0X" <> hexitP
    ]

litFloatP = Tlex.orP
    [ decimalP <> chP '.' <> decimalP <> Tlex.maybeP exponentP
    , decimalP <> exponentP
    ]

exponentP = charsP ['e', 'E'] <> Tlex.maybeP (charsP ['+', '-']) <> decimalP

litCharP = chP '\'' <> Tlex.orP [graphicWithoutSpP, spaceP, charEscapeP] <> chP '\'' where
    graphicWithoutSpP = charSetP
        $ graphicCs `CharSet.difference` CharSet.fromList ['\'', '\\']
litStringP = chP '"' <> Tlex.manyP (Tlex.orP [graphicWithoutSpP, spaceP, escapeP, gapP]) <> chP '"' where
    graphicWithoutSpP = charSetP
        $ graphicCs `CharSet.difference` CharSet.fromList ['"', '\\']
charEscapeP = escapeBaseP charescWithoutAmpP
escapeP = escapeBaseP charescP
escapeBaseP p = chP '\\' <> Tlex.orP
    [ p
    , asciiP
    , decimalP
    , chP 'o' <> octalP
    , chP 'x' <> hexadecimalP
    ]
charescWithoutAmpP = charsP ['a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '"', '\'']
charescP = Tlex.orP [charescWithoutAmpP, chP '&']
asciiP = Tlex.orP
    [ chP '^' <> cntrlP
    , stringP "NUL"
    , stringP "SOH"
    , stringP "STX"
    , stringP "ETX"
    , stringP "EOT"
    , stringP "ENQ"
    , stringP "ACK"
    , stringP "BEL"
    , stringP "BS"
    , stringP "HT"
    , stringP "LF"
    , stringP "LF"
    , stringP "VT"
    , stringP "FF"
    , stringP "CR"
    , stringP "SO"
    , stringP "SI"
    , stringP "DLE"
    , stringP "DC1"
    , stringP "DC2"
    , stringP "DC3"
    , stringP "DC4"
    , stringP "NAK"
    , stringP "SYN"
    , stringP "ETB"
    , stringP "CAN"
    , stringP "EM"
    , stringP "SUB"
    , stringP "ESC"
    , stringP "FS"
    , stringP "GS"
    , stringP "RS"
    , stringP "US"
    , stringP "SP"
    , stringP "DEL"
    ]
cntrlP = Tlex.orP
    [ ascLargeP
    , chP '@'
    , chP '['
    , chP '\\'
    , chP ']'
    , chP '^'
    , chP '_'
    ]
gapP = chP '\\' <> Tlex.someP whitecharP <> chP '\\'


charSetP :: CharSet.CharSet -> Pattern
charSetP cs = TlexEnc.charSetP TlexEnc.charSetPUtf8 cs

chP :: Char -> Pattern
chP c = TlexEnc.chP TlexEnc.charSetPUtf8 c

charsP :: [Char] -> Pattern
charsP cs = TlexEnc.charsP TlexEnc.charSetPUtf8 cs

stringP :: String -> Pattern
stringP s = TlexEnc.stringP TlexEnc.charSetPUtf8 s
