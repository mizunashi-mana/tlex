module Language.Lexer.Tlex.Pipeline.Dfa2Runner (
    dfa2Runner,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.IntMap                         as IntMap
import qualified Data.EnumMap.Strict    as EnumMap
import qualified Language.Lexer.Tlex.Machine.DFA     as DFA
import qualified Language.Lexer.Tlex.Machine.Pattern as Pattern
import qualified Language.Lexer.Tlex.Machine.State   as MState
import qualified Language.Lexer.Tlex.Runner          as Tlex


dfa2Runner :: Enum e => DFA.DFA a -> Tlex.Runner e a
dfa2Runner dfa = Tlex.Runner
    { tlexInitial = dfaTlexInitial
    , tlexAccept = dfaTlexAccept
    , tlexTrans = dfaTlexTrans
    }
    where
        dfaTlexInitial s0 =
            let ms = EnumMap.lookup
                    do toEnum s0
                    do DFA.dfaInitials dfa
            in case ms of
                Nothing -> -1
                Just s  -> fromEnum s

        dfaTlexAccept s0 =
            let dstState = MState.indexArray
                    do DFA.dfaTrans dfa
                    do toEnum s0
            in case DFA.dstAccepts dstState of
                []    -> Nothing
                acc:_ -> Just do Pattern.accSemanticAction acc

        dfaTlexTrans s0 c =
            let dstState = MState.indexArray
                    do DFA.dfaTrans dfa
                    do toEnum s0
            in case IntMap.lookup c do DFA.dstTrans dstState of
                Just s1 -> fromEnum s1
                Nothing -> case DFA.dstOtherTrans dstState of
                    Just s1 -> fromEnum s1
                    Nothing -> -1
