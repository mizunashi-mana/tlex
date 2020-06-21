module Main where

import qualified Language.Lexer.Tlex as Tlex
import qualified Data.CharSet         as CharSet
import qualified Data.CharSet.Unicode as UniCharSet

main :: IO ()
main = putStrLn "Hello, Haskell!"

data LexerState
    = Initial
    | NestedComment
    deriving (Eq, Show, Enum)

initialRule
    :: Tlex.Pattern -> Tlex.SemanticAction LexerState a
    -> Tlex.LexerRuleBuilder LexerState a ()
initialRule = Tlex.lexerRule [Initial]

lexer :: Tlex.LexerDeclaration a
lexer = Tlex.LexerDeclaration
    { initialState = Initial
    , rules = rules
    }

lexerRules :: LexerRuleBuilder LexerState a ()
lexerRules = do
    initialRule whitecharP action

    initialRule commentP action

    initialRule openComP action
    -- closeComP should be the head on nested comment mode to avoid conflicting.
    Tlex.lexerRule [NestedComment] closeComP action
    Tlex.lexerRule [NestedComment] anyWithNewlineP action

    initialRule specialP action

    -- reservedIdP should be before qvarid to avoid conflicting.
    initialRule reservedIdP action
    -- reservedOpP should be before qvarsym / qconsym to avoid conflicting.
    initialRule reservedOpP action

    initialRule qvaridP action
    initialRule qconidP action
    initialRule qvarsymP action
    initialRule qconsymP action

    initialRule litIntegerP action
    initialRule litFloatP action
    initialRule litCharP action
    initialRule litStringP action
    where
        action = Tlex.SemanticAction id


-- See https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-160002.2

specialP = charSetP specialCs
specialCs = CharSet.fromList
    ['(', ')', ',', ';', '[', ']', '`', '{' '}']

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
    anyWithoutSymbolP = charSetP do anyCs `CharSet.difference` symbolCs
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
ascSmall = CharSet.range 'a' 'z'
uniSmall = UniCharSet.lowercaseLetter

largeCs = mconcat
    [ ascLargeCs
    , uniLargeCs
    ]
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

varidP = smallP <> Tlex.orP [smallP, largeP, digitP, chP '\'']
conidP = largeP <> Tlex.orP [smallP, largeP, digitP, chP '\'']
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

exponentP = Tlex.charsP ['e', 'E'] <> Tlex.maybeP (Tlex.charsP ['+', '-']) <> decimalP

litCharP = chP '\'' <> Tlex.orP [graphicWithoutSpP, spaceP, charEscapeP] <> chP '\'' where
    graphicWithoutSpP = charSetP
        $ graphicCs `CharSet.difference` CharSet.fromList ['\'', '\\']
litStringP = chP '"' <> Tlex.manyP (Tlex.orP [graphicWithoutSpP, spaceP, escapeP, gapP]) <> '"' where
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


charSetP :: CharSet.CharSet -> Tlex.Pattern
charSetP cs = Tlex.Range cs

chP :: Char -> Tlex.Pattern
chP c = Tlex.Range $ CharSet.singleton c

charsP :: [Char] -> Tlex.Pattern
charsP cs = charSetP $ CharSet.fromList cs

stringP :: String -> Tlex.Pattern
stringP s = foldMap chP s
