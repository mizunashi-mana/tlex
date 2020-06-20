module Language.Lexer.Tlex.Machine.NFA
    (
        NFA,
        NFAState(..),
        NFABuilder,
        buildNFA,
        epsilonClosed,
        newStateNum,
        pattern2Nfa,
    ) where

import Language.Lexer.Tlex.Prelude

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Array as Array
import qualified Language.Lexer.Tlex.Syntax as TlexSyntax
import qualified Language.Lexer.Tlex.Data.CharSet as Tlex
import qualified Language.Lexer.Tlex.Data.Graph as Graph


newtype NFA = NFA (Array.Array TlexSyntax.StateNum NFAState)

-- |
--
-- TODO:
-- * support polymorphic trans condition; only support charset now.
--
data NFAState = NState
    { nstAccepts :: [TlexSyntax.Accept ()]
    , nstEpsilonTrans :: [TlexSyntax.StateNum]
    , nstTrans :: [(Tlex.CharSet, TlexSyntax.StateNum)]
    }
    deriving (Eq, Show)

epsilonClosed :: NFA -> NFA
epsilonClosed (NFA arr) = NFA $ Array.listArray
        do Array.bounds arr
        do [ go v s | (v, s) <- Array.assocs arr ]
    where
        go v s = s
            { nstEpsilonTrans = gr Array.! v
            }

        gr = Graph.transClosure
            do fmap nstEpsilonTrans arr


type MapNFA = IntMap.IntMap NFAState

newtype NFABuilder a = NFABuilder
    { unNFABuilder
        :: TlexSyntax.StateNum -> MapNFA
        -> (TlexSyntax.StateNum, MapNFA, a)
    }
    deriving Functor

buildNFA :: NFABuilder () -> NFA
buildNFA (NFABuilder builder) =
    let (s, m, ()) = builder 0 IntMap.empty
        arr = Array.array (0, s - 1) do IntMap.toAscList m
    in epsilonClosed do NFA arr

newStateNum :: NFABuilder TlexSyntax.StateNum
newStateNum = NFABuilder \s m -> (succ s, m, s)

instance Applicative NFABuilder where
    pure x = NFABuilder \s0 m0 -> (s0, m0, x)
    NFABuilder bf <*> NFABuilder bx = NFABuilder \s0 m0 ->
        let (s1, m1, f) = bf s0 m0
            (s2, m2, x) = bx s1 m1
        in (s2, m2, f x)

instance Monad NFABuilder where
    NFABuilder bx >>= k = NFABuilder \s0 m0 ->
        let (s1, m1, x) = bx s0 m0 in unNFABuilder (k x) s1 m1


pattern2Nfa
    :: TlexSyntax.StateNum -> TlexSyntax.StateNum
    -> TlexSyntax.Pattern -> NFABuilder ()
pattern2Nfa = go where
    go b e = \case
        TlexSyntax.Empty -> epsilonTrans b e
        TlexSyntax.AnyOne -> undefined
        TlexSyntax.Range cs -> undefined cs
        p1 TlexSyntax.:^: p2 -> undefined p1 p2
        p1 TlexSyntax.:|: p2 -> undefined p1 p2
        TlexSyntax.Maybe p -> undefined p
        TlexSyntax.Some p -> undefined p
        TlexSyntax.Many p -> undefined p

epsilonTrans :: TlexSyntax.StateNum -> TlexSyntax.StateNum -> NFABuilder ()
epsilonTrans sf st
    | sf == st  = pure ()
    | otherwise = NFABuilder \s0 n0 ->
        let n1 = addEpsTrans n0 in (s0, n1, ())
    where
        addEpsTrans n = case IntMap.lookup sf n of
            Nothing -> IntMap.insert sf
                do NState
                    { nstAccepts = []
                    , nstEpsilonTrans = [st]
                    , nstTrans = []
                    }
                do n
            Just s@NState{ nstEpsilonTrans } -> IntMap.insert sf
                do s { nstEpsilonTrans = st:nstEpsilonTrans }
                do n

