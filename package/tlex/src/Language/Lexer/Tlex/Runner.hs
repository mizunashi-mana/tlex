module Language.Lexer.Tlex.Runner (
    TlexContext (..),
    TlexResult (..),
    Runner (..),
    runRunner,
) where

import           Language.Lexer.Tlex.Prelude


class (Enum e, Monad m) => TlexContext p e m | m -> p, m -> e where
    tlexGetInputPart :: m (Maybe e)
    tlexGetMark :: m p

data TlexResult p a
    = TlexEndOfInput
    | TlexError
    | TlexAccepted p a
    deriving (Eq, Show)


data Runner e a = Runner
    { tlexInitial :: Int -> Int
    -- ^ StartState -> (StateNum | -1)
    , tlexAccept  :: Int -> Maybe a
    -- ^ StateNum -> Maybe Action
    , tlexTrans   :: Int -> Int -> Int
    -- ^ StateNum -> CodeUnit -> (StateNum | -1)
    }
    deriving Functor

runRunner :: Enum s => TlexContext p c m => Runner c a -> s -> m (TlexResult p a)
runRunner runner s0 = case tlexInitial runner do fromEnum s0 of
        -1 -> error "unknown initial state"
        s  -> go s
    where
        go s = case tlexAccept runner s of
            Just x  -> do
                acc <- buildAccepted x
                mc <- tlexGetInputPart
                case mc of
                    Nothing -> pure acc
                    Just c  -> goTrans s c do Just acc
            Nothing -> do
                mc <- tlexGetInputPart
                case mc of
                    Nothing -> pure TlexEndOfInput
                    Just c  -> goTrans s c Nothing

        goTrans s c preAccepted = case tlexTrans runner s do fromEnum c of
            -1 -> goEnd preAccepted
            ns -> do
                nacc <- case tlexAccept runner ns of
                    Just x -> do
                        acc <- buildAccepted x
                        pure do Just acc
                    Nothing -> pure preAccepted
                mc <- tlexGetInputPart
                case mc of
                    Nothing -> goEnd nacc
                    Just nc -> goTrans ns nc nacc

        buildAccepted x = do
            m <- tlexGetMark
            pure do TlexAccepted m x

        goEnd preAccepted = case preAccepted of
            Nothing  -> pure TlexError
            Just acc -> pure acc
