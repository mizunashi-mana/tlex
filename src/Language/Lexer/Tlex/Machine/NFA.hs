module Language.Lexer.Tlex.Machine.NFA
    (
        NFA,
        NFAState(..),
        NFABuilder,
        buildNFA,
        epsilonClosed,
        newStateNum,
        epsilonTrans,
        condTrans,
        accept,
        initial,
    ) where

import Language.Lexer.Tlex.Prelude

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Array as Array
import qualified Language.Lexer.Tlex.Syntax as Tlex
import qualified Language.Lexer.Tlex.Data.CharSet as CharSet
import qualified Language.Lexer.Tlex.Data.Graph as Graph


data NFA s = NFA
    { nfaInitialStates :: [(Tlex.StateNum, s)]
    , nfaTrans :: Array.Array Tlex.StateNum (NFAState s)
    }
    deriving (Eq, Show)

-- |
--
-- TODO:
-- * support polymorphic trans condition; only support charset now.
--
data NFAState s = NState
    { nstAccepts :: [Tlex.Accept s]
    , nstEpsilonTrans :: [Tlex.StateNum]
    , nstTrans :: [(CharSet.CharSet, Tlex.StateNum)]
    }
    deriving (Eq, Show)

epsilonClosed :: NFA s -> NFA s
epsilonClosed nfa@NFA{ nfaTrans } = nfa
    { nfaTrans = Array.listArray
        do Array.bounds nfaTrans
        do [ go v s | (v, s) <- Array.assocs nfaTrans ]
    }
    where
        go v s = s
            { nstEpsilonTrans = gr Array.! v
            }

        gr = Graph.transClosure
            do fmap nstEpsilonTrans nfaTrans


type MapNFA s = IntMap.IntMap (NFAState s)

newtype NFABuilder s a = NFABuilder
    { unNFABuilder
        :: [(Tlex.StateNum, s)] -> Tlex.StateNum -> MapNFA s
        -> ([(Tlex.StateNum, s)], Tlex.StateNum, MapNFA s, a)
    }
    deriving Functor

buildNFA :: NFABuilder s () -> NFA s
buildNFA (NFABuilder builder) =
    let (is, s, m, ()) = builder [] 0 IntMap.empty
        arr = Array.array (0, s - 1) do IntMap.toAscList m
    in epsilonClosed do NFA is arr

newStateNum :: NFABuilder s Tlex.StateNum
newStateNum = NFABuilder \is0 s0 m0 -> (is0, succ s0, m0, s0)

instance Applicative (NFABuilder s) where
    pure x = NFABuilder \is0 s0 m0 -> (is0, s0, m0, x)
    NFABuilder bf <*> NFABuilder bx = NFABuilder \is0 s0 m0 ->
        let (is1, s1, m1, f) = bf is0 s0 m0
            (is2, s2, m2, x) = bx is1 s1 m1
        in (is2, s2, m2, f x)

instance Monad (NFABuilder s) where
    NFABuilder bx >>= k = NFABuilder \is0 s0 m0 ->
        let (is1, s1, m1, x) = bx is0 s0 m0
        in unNFABuilder (k x) is1 s1 m1

epsilonTrans :: Tlex.StateNum -> Tlex.StateNum -> NFABuilder s ()
epsilonTrans sf st
    | sf == st  = pure ()
    | otherwise = NFABuilder \is0 s0 n0 ->
        let n1 = addEpsTrans n0 in (is0, s0, n1, ())
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

condTrans
    :: Tlex.StateNum -> CharSet.CharSet -> Tlex.StateNum -> NFABuilder s ()
condTrans sf r st = NFABuilder \is0 s0 n0 ->
    let n1 = addCondTrans n0 in (is0, s0, n1, ())
    where
        addCondTrans n = case IntMap.lookup sf n of
            Nothing -> IntMap.insert sf
                do NState
                    { nstAccepts = []
                    , nstEpsilonTrans = []
                    , nstTrans = [(r, st)]
                    }
                do n
            Just s@NState{ nstTrans } -> IntMap.insert sf
                do s
                    { nstTrans = (r, st):nstTrans
                    }
                do n

accept :: Tlex.StateNum -> Tlex.Accept s -> NFABuilder s ()
accept s x = NFABuilder \is0 s0 n0 ->
    let n1 = addAccept n0 in (is0, s0, n1, ())
    where
        addAccept n = case IntMap.lookup s n of
            Nothing -> IntMap.insert s
                do NState
                    { nstAccepts = [x]
                    , nstEpsilonTrans = []
                    , nstTrans = []
                    }
                do n
            Just ns@NState { nstAccepts } -> IntMap.insert s
                do ns
                    { nstAccepts = x:nstAccepts
                    }
                do n

initial :: Tlex.StateNum -> s -> NFABuilder s ()
initial s x = NFABuilder \is0 s0 n0 -> ((s, x):is0, s0, n0, ())
