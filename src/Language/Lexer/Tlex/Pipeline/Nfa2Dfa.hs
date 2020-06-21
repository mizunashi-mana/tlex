module Language.Lexer.Tlex.Pipeline.Nfa2Dfa where

import qualified Language.Lexer.Tlex.Machine.NFA as NFA
import qualified Language.Lexer.Tlex.Machine.DFA as DFA


nfa2Dfa :: NFA.NFA s a -> DFA.DFA s a
nfa2Dfa = nfa2Dfa
