module Language.Lexer.Tlex.Runner (
    TlexContext (..),
    TlexResult (..),
    Runner (..),
    runRunner,
) where

import           Language.Lexer.Tlex.Prelude


class (Enum unit, Monad m) => TlexContext mark unit m | m -> mark, m -> unit where
    -- | Get a unit of current position from input, and move to next position.
    tlexGetInputPart :: m (Maybe unit)

    -- | Get a mark of current position.
    tlexGetMark :: m mark

data TlexResult mark action
    = TlexEndOfInput
    -- ^ No more inputs.
    | TlexError
    -- ^ Some inputs are available, but not accepted.
    | TlexAccepted mark action
    -- ^ Accepted with a end position and an action.
    deriving (Eq, Show)


data Runner unit action = Runner
    { tlexInitial :: Int -> Int
    -- ^ StartState -> (StateNum | -1)
    , tlexAccept  :: Int -> Maybe action
    -- ^ StateNum -> Maybe Action
    , tlexTrans   :: Int -> Int -> Int
    -- ^ StateNum -> CodeUnit -> (StateNum | -1)
    }
    deriving Functor

runRunner :: Enum state => TlexContext mark unit m
    => Runner unit action -> state -> m (TlexResult mark action)
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
