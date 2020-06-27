module Language.Lexer.Tlex.Pipeline.Nfa2Dfa (
    nfa2Dfa,
) where

import Language.Lexer.Tlex.Prelude

import qualified Data.Hashable as Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Machine.NFA as NFA
import qualified Language.Lexer.Tlex.Machine.DFA as DFA
import qualified Language.Lexer.Tlex.Syntax as Tlex


nfa2Dfa :: NFA.NFA s a -> DFA.DFA s a
nfa2Dfa nfa = nfa2Dfa nfa


data Nfa2DfaContext s m = Nfa2DfaContext
    { nfa2DfaCtxStateMap :: HashMap.HashMap MState.StateSet MState.StateNum
    , nfa2DfaCtxDFABuilderCtx :: DFA.DFABuilderContext s m
    }

type Nfa2DfaM s m = State (Nfa2DfaContext s m)


