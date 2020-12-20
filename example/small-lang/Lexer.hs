{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Lexer where

import qualified Data.ByteString     as ByteString
import qualified GHC.Word
import qualified Language.Lexer.Tlex as Tlex
import qualified Lexer.Rules


$(Lexer.Rules.buildLexer)

lexByteString :: ByteString.ByteString -> Either String [ByteString.ByteString]
lexByteString input = go (ByteString.unpack input) id where
    go s acc = case Tlex.runInputString (tlexScan ()) s of
        (Tlex.TlexEndOfInput, _)      -> Right $ acc []
        (Tlex.TlexError, ctx)         -> Left $ show (ctx, acc [])
        (Tlex.TlexAccepted ctx (), _) ->
            let rest = Tlex.inputStringCtxRest ctx
                consumed = Tlex.inputStringCtxPos ctx
            in go rest (\n -> acc (ByteString.pack (take consumed s):n))
