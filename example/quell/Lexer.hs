{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Lexer where

import qualified Data.ByteString     as ByteString
import qualified Data.Word           as Word
import qualified Language.Lexer.Tlex as Tlex
import qualified Lexer.Rules         as Rules
import qualified Lexer.Token         as Token
import qualified Lexer.CodeUnit         as CodeUnit


$(Rules.buildLexer)

data SpannedAction = SpannedAction
    { lexerAction   :: Rules.LexerAction
    , rawCodeUnits  :: [CodeUnit.T]
    , tokenSpan     :: (Int, Int)
    }
    deriving (Eq, Show)

data LexerContext = LexerContext
    { currentPosition  :: Int
    , restString       :: [CodeUnit.T]
    }
    deriving (Eq, Show)

lexString :: String -> Either String [SpannedAction]
lexString input = go lctx0 id where
    lctx0 = LexerContext
        { currentPosition = 0
        , restString = [ CodeUnit.fromChar c | c <- input ]
        }

    initialState lctx = Rules.Initial

    go lctx acc =
        let istate = initialState lctx
            s = restString lctx
        in case Tlex.runInputString (tlexScan istate) s of
            (Tlex.TlexEndOfInput, _)      -> Right $ acc []
            (Tlex.TlexError, ctx)         -> Left $ show (ctx, acc [])
            (Tlex.TlexAccepted ctx act, _) ->
                let consumed = Tlex.inputStringCtxPos ctx
                    consumedString = take consumed s
                    consumedToken = SpannedAction
                        { lexerAction = act
                        , rawCodeUnits = consumedString
                        , tokenSpan = (currentPosition lctx, consumed)
                        }
                    nlctx = LexerContext
                        { currentPosition = currentPosition lctx + consumed
                        , restString = Tlex.inputStringCtxRest ctx
                        }
                in go nlctx $ \n -> acc $ consumedToken : n
