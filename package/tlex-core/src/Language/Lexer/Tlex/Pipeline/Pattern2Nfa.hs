module Language.Lexer.Tlex.Pipeline.Pattern2Nfa (
    pattern2Nfa,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Data.EnumSet    as EnumSet
import qualified Language.Lexer.Tlex.Data.SymEnumSet as SymEnumSet
import qualified Language.Lexer.Tlex.Machine.NFA     as NFA
import qualified Language.Lexer.Tlex.Machine.State   as MState
import qualified Language.Lexer.Tlex.Machine.Pattern as Pattern


pattern2Nfa
    :: Enum e
    => MState.StateNum -> MState.StateNum -> Pattern.Pattern e
    -> NFA.NFABuilder m ()
pattern2Nfa = go where
    go b e = \case
        Pattern.Empty -> NFA.epsilonTrans b e
        Pattern.Range s -> NFA.condTrans b
            do
                let (isStraight, es) = SymEnumSet.toEnumSet s
                NFA.NFAStateTrans
                    { NFA.nstTransIsStraight = isStraight
                    , NFA.nstTransRange = EnumSet.toIntSet es
                    , NFA.nstTransNextState = e
                    }
        p1 Pattern.:^: p2 -> do
            s <- NFA.newStateNum
            pattern2Nfa b s p1
            pattern2Nfa s e p2
        p1 Pattern.:|: p2 -> do
            pattern2Nfa b e p1
            pattern2Nfa b e p2
        Pattern.Many p -> do
            s <- NFA.newStateNum
            NFA.epsilonTrans b s
            pattern2Nfa s s p
            NFA.epsilonTrans s e
