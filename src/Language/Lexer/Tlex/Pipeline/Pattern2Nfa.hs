module Language.Lexer.Tlex.Pipeline.Pattern2Nfa (
    pattern2Nfa,
    scanRule2Nfa,
    scanner2Nfa,
) where

import Language.Lexer.Tlex.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Hashable as Hashable
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

scanner2Nfa :: Eq s => Hashable.Hashable s => Tlex.Scanner s m -> NFA.NFABuilder s m ()
scanner2Nfa Tlex.Scanner{ scannerRules } = foldM_
    do \(p, bs, is) scanRule -> aggScanRule p bs is scanRule
    do (Tlex.mostPriority, [], HashMap.empty)
    do scannerRules
    where
        aggScanRule p0 bs0 is0 scanRule = do
            b <- NFA.newStateNum
            scanRule2Nfa p0 b scanRule
            is1 <- registerStartState is0 b do Tlex.scanRuleStartState scanRule
            pure (succ p0, b:bs0, is1)

        registerStartState is0 b ss = foldM
            do \is s -> do
                (is', sn) <- case HashMap.lookup s is of
                    Just x  -> pure (is, x)
                    Nothing -> do
                        x <- NFA.newStateNum
                        NFA.initial x s
                        pure (HashMap.insert s x is, x)
                NFA.epsilonTrans sn b
                pure is'
            do is0
            do ss
