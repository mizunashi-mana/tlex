module Language.Lexer.Tlex.Output.TH (
    outputDfa,
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Machine.DFA as DFA


outputDfa :: forall s a. TH.Lift s => DFA.DFA s a -> TH.Q TH.Dec
outputDfa dfa = do
    let fName = TH.mkName "lex"
    clause <- clauseQ
    pure do TH.FunD fName [clause]
    where
        clauseQ = undefined (outputDfaState @s) dfa

{-

-}
outputDfaState :: TH.Lift s => DFA.DFAState s a -> TH.Q TH.Exp
outputDfaState dfaState = undefined dfaState stateName

stateName :: MState.StateNum -> TH.Name
stateName s = TH.mkName do 's':show do fromEnum s
