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
    | TlexSkip
    | TlexAccepted a

{-
tlexScan :: Enum s => TlexContext m => s -> m (TlexResult a)
tlexScan s0 = go where
    go = do
        mc <- tlexGetInputPart
        goTrans (tlexInitialState s0)

    goTrans s = 
        
        case tlexTrans s c of
            -1 -> 
            ns -> goTrans ns

tlexInitialState :: Enum s => s -> Int
tlexInitialState x = case fromEnum x of
    1 -> ...

tlexTrans :: Int -> Char -> Int
tlexTrans sf c = case sf of
    1 -> case c of
        'a' -> 2
        ...
        _ -> 4
    ...
    _ -> -1

tlexAccept :: s -> 
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
