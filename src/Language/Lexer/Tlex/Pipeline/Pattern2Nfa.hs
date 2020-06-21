module Language.Lexer.Tlex.Pipeline.Pattern2Nfa where

import Language.Lexer.Tlex.Prelude

import Language.Lexer.Tlex.Syntax as Tlex
import Language.Lexer.Tlex.Machine.NFA as NFA


pattern2Nfa
    :: Tlex.StateNum -> Tlex.StateNum -> Tlex.Pattern -> NFA.NFABuilder s m ()
pattern2Nfa = go where
    go b e = \case
        Tlex.Empty -> NFA.epsilonTrans b e
        Tlex.Range cs -> NFA.condTrans b cs e
        p1 Tlex.:^: p2 -> do
            s <- NFA.newStateNum
            pattern2Nfa b s p1
            pattern2Nfa s e p2
        p1 Tlex.:|: p2 -> do
            pattern2Nfa b e p1
            pattern2Nfa b e p2
        Tlex.Many p -> do
            s <- NFA.newStateNum
            NFA.epsilonTrans b s
            pattern2Nfa s s p
            NFA.epsilonTrans s e

scanRule2Nfa
    :: Tlex.AcceptPriority -> Tlex.StateNum -> Tlex.ScanRule s m
    -> NFA.NFABuilder s m ()
scanRule2Nfa p b r = do
    e <- NFA.newStateNum
    pattern2Nfa b e do Tlex.scanRulePattern r

    NFA.accept e $ Tlex.Accept
        { accPriority = p
        , accSemanticAction = Tlex.scanRuleSemanticAction r
        }
