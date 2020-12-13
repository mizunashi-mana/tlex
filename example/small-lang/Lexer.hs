{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Lexer where

import qualified Language.Lexer.Tlex.Plugin.TH as TlexTH
import qualified Lexer.Rules
import qualified GHC.Word
import qualified Data.ByteString               as ByteString


$(Lexer.Rules.buildLexer)

lexByteString :: ByteString.ByteString -> Either String [ByteString.ByteString]
lexByteString input = go (ByteString.unpack input) id where
    go s acc = case TlexTH.runInputString (tlexScan ()) s of
        (TlexTH.TlexEndOfInput, _)      -> Right $ acc []
        (TlexTH.TlexError, ctx)         -> Left $ show (ctx, acc [])
        (TlexTH.TlexAccepted ctx (), _) ->
            let rest = TlexTH.inputStringCtxRest ctx
                consumed = TlexTH.inputStringCtxPos ctx
            in go rest (\n -> acc (ByteString.pack (take consumed s):n))
