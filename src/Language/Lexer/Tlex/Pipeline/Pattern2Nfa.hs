module Language.Lexer.Tlex.Pipeline.Pattern2Nfa (
    pattern2Nfa,
    scanRule2Nfa,
    scanner2Nfa,
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Syntax as Tlex
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Machine.NFA as NFA


pattern2Nfa
    :: MState.StateNum -> MState.StateNum -> Tlex.Pattern -> NFA.NFABuilder s m ()
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
    :: Tlex.AcceptPriority -> MState.StateNum -> Tlex.ScanRule s m -> NFA.NFABuilder s m ()
scanRule2Nfa p b r = do
    e <- NFA.newStateNum
    pattern2Nfa b e do Tlex.scanRulePattern r

    NFA.accept e $ Tlex.Accept
        { accPriority = p
        , accSemanticAction = Tlex.scanRuleSemanticAction r
        }

scanner2Nfa :: [s] -> Tlex.Scanner s m -> NFA.NFABuilder s m ()
scanner2Nfa ss Tlex.Scanner{ scannerRules } = do
    is <- forM ss \s -> do
        sn <- NFA.newStateNum
        NFA.initial sn s
        pure sn

    let agg (p, bs) scanRule = do
            b <- NFA.newStateNum
            scanRule2Nfa p b scanRule
            pure (succ p, b:bs)
    (_, bs) <- foldM agg (Tlex.mostPriority, []) scannerRules

    forM_ is \i ->
        forM_ bs \b ->
            NFA.epsilonTrans i b

    pure ()
