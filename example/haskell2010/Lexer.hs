{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Lexer where

import qualified Data.ByteString     as ByteString
import qualified Data.Word           as Word
import qualified Language.Lexer.Tlex as Tlex
import qualified Lexer.Rules         as LexerRules


$(LexerRules.buildLexer)

data SpannedToken = SpannedToken
    { token         :: LexerRules.Token
    , rawByteString :: ByteString.ByteString
    , tokenSpan     :: (Int, Int)
    }
    deriving (Eq, Show)

data LexerContext = LexerContext
    { commentNestLevel :: Int
    , currentPosition  :: Int
    , restString       :: [Word.Word8]
    }
    deriving (Eq, Show)

lexByteString :: ByteString.ByteString -> Either String [SpannedToken]
lexByteString input = go lctx0 id where
    lctx0 = LexerContext
        { commentNestLevel = 0
        , currentPosition = 0
        , restString = ByteString.unpack input
        }

    initialState lctx = case commentNestLevel lctx of
        0 -> LexerRules.Initial
        _ -> LexerRules.NestedComment

    go lctx acc =
        let istate = initialState lctx
            s = restString lctx
        in case Tlex.runInputString (tlexScan istate) s of
            (Tlex.TlexEndOfInput, _)      -> Right $ acc []
            (Tlex.TlexError, ctx)         -> Left $ show (ctx, acc [])
            (Tlex.TlexAccepted ctx act, _) ->
                let consumed = Tlex.inputStringCtxPos ctx
                    consumedString = ByteString.pack $ take consumed s
                    consumedToken = SpannedToken
                        { token = act consumedString
                        , rawByteString = consumedString
                        , tokenSpan = (currentPosition lctx, consumed)
                        }
                    nlctx = LexerContext
                        { commentNestLevel = case token consumedToken of
                            LexerRules.TokOpenComment  -> commentNestLevel lctx + 1
                            LexerRules.TokCloseComment -> commentNestLevel lctx - 1
                            _                          -> commentNestLevel lctx
                        , currentPosition = currentPosition lctx + consumed
                        , restString = Tlex.inputStringCtxRest ctx
                        }
                in go nlctx $ \n -> acc $ consumedToken : n
