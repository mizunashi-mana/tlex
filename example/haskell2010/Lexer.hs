{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Lexer where

import qualified Data.ByteString     as ByteString
import qualified Language.Lexer.Tlex as Tlex
import qualified Lexer.Rules         as LexerRules


$(LexerRules.buildLexer)

data SpannedToken = SpannedToken
    { token         :: LexerRules.Token
    , rawByteString :: ByteString.ByteString
    , tokenSpan     :: (Int, Int)
    }
    deriving (Eq, Show)

lexByteString :: ByteString.ByteString -> Either String [SpannedToken]
lexByteString input = go 0 (ByteString.unpack input) id where
    go i s acc = case Tlex.runInputString (tlexScan LexerRules.Initial) s of
        (Tlex.TlexEndOfInput, _)      -> Right $ acc []
        (Tlex.TlexError, ctx)         -> Left $ show (ctx, acc [])
        (Tlex.TlexAccepted ctx act, _) ->
            let rest = Tlex.inputStringCtxRest ctx
                consumed = Tlex.inputStringCtxPos ctx
                consumedString = ByteString.pack $ take consumed s
                consumedToken = SpannedToken
                    { token = act consumedString
                    , rawByteString = consumedString
                    , tokenSpan = (i, consumed)
                    }
            in go (i + consumed) rest $ \n -> acc $ consumedToken : n
