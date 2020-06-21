module Language.Lexer.Tlex.Machine.DFA where

import qualified Data.IntMap as IntMap
import qualified Language.Lexer.Tlex.Data.CharMap as CharMap
import qualified Language.Lexer.Tlex.Syntax as Tlex


data DFA s a = DFA
    { dfaInitials :: [(Tlex.StateNum, s)]
    , dfaTrans :: IntMap.IntMap (DFAState s a)
    }

-- |
--
-- TODO:
-- * support byte map transition.
--
data DFAState s a = DState
    { dstAccepts :: [Tlex.Accept s a]
    , dstTrans :: CharMap.CharMap Tlex.StateNum
    }
