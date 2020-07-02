module Language.Lexer.Tlex.Output.TH (
    TlexContext (..),
    TlexResult (..),
    outputDfa,
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Haskell.TH as TH
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Machine.DFA as DFA


class Monad m => TlexContext m where
    tlexGetInputPart :: m (Maybe Char)

data TlexResult a
    = TlexEndOfInput
    | TlexError
    | TlexAccepted a

{-
type SemanticAction = ...

tlexScan :: Enum s => TlexContext m => s -> m (TlexResult SemanticAction)
tlexScan s0 = go (tlexInitialState s0) where
    go s = case tlexAccept s of
        Just x  -> pure (TlexAccepted x)
        Nothing -> do
            mc <- tlexGetInputPart
            case mc of
                Nothing -> pure TlexEndOfInput
                Just c  -> goTrans s c

    goTrans s c =  case tlexTrans s c of
        -1 -> pure TlexError
        ns -> do
            mc <- tlexGetInputPart
            case mc of
                Nothing -> pure TlexError
                Just c  -> goTrans ns c

tlexInitialState :: Enum s => s -> Int
tlexInitialState x = case fromEnum x of
    1 -> 10
    ...
    _ -> error "unavailable start state"

tlexTrans :: Int -> Char -> Int
tlexTrans sf c = case sf of
    1 -> case c of
        'a' -> 2
        ...
        _ -> 4
    ...
    _ -> -1

tlexAccept :: Int -> Maybe SemanticAction
tlexAccept i = case i of
    1 -> Just ...
    ...
    _ -> Nothing
-}
outputDfa :: forall a. DFA.DFA a -> TH.Q TH.Dec
outputDfa dfa = do
    let fName = TH.mkName "lex"
    clause <- clauseQ
    pure do TH.FunD fName [clause]
    where
        clauseQ = undefined outputDfaState dfa

outputDfaState :: DFA.DFAState a -> TH.Q TH.Exp
outputDfaState dfaState = undefined dfaState stateName

stateName :: MState.StateNum -> TH.Name
stateName s = TH.mkName do 's':show do fromEnum s
