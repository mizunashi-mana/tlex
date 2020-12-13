module Language.Lexer.Tlex.Pipeline.Scanner2Nfa (
    scanRule2Nfa,
    scanner2Nfa,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Data.EnumMap    as EnumMap
import qualified Language.Lexer.Tlex.Machine.NFA     as NFA
import qualified Language.Lexer.Tlex.Machine.State   as MState
import qualified Language.Lexer.Tlex.Machine.Pattern as Pattern
import qualified Language.Lexer.Tlex.Pipeline.Pattern2Nfa as Pattern2Nfa
import qualified Language.Lexer.Tlex.Syntax          as Tlex


scanRule2Nfa
    :: Enum e
    => Pattern.AcceptPriority -> MState.StateNum -> Tlex.ScanRule e m
    -> NFA.NFABuilder m ()
scanRule2Nfa p b r = do
    e <- NFA.newStateNum
    Pattern2Nfa.pattern2Nfa b e do Tlex.scanRulePattern r

    NFA.accept e
        do Tlex.Accept
            { accPriority = p
            , accSemanticAction = Tlex.scanRuleSemanticAction r
            }

scanner2Nfa :: Enum e => Tlex.Scanner e m -> NFA.NFABuilder m ()
scanner2Nfa Tlex.Scanner{ scannerRules } = foldM_
    do \(p, bs, is) scanRule -> aggScanRule p bs is scanRule
    do (Pattern.mostPriority, [], EnumMap.empty)
    do scannerRules
    where
        aggScanRule p0 bs0 is0 scanRule = do
            b <- NFA.newStateNum
            scanRule2Nfa p0 b scanRule
            is1 <- registerStartState is0 b do Tlex.scanRuleStartStates scanRule
            pure (succ p0, b:bs0, is1)

        registerStartState is0 b ss = foldM
            do \is s -> do
                (is', sn) <- case EnumMap.lookup s is of
                    Just x  -> pure (is, x)
                    Nothing -> do
                        x <- NFA.newStateNum
                        NFA.initial x s
                        pure (EnumMap.insert s x is, x)
                NFA.epsilonTrans sn b
                pure is'
            do is0
            do ss
