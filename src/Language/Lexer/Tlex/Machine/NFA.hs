module Language.Lexer.Tlex.Machine.NFA
    (
        NFA (..),
        NFAState(..),
        NFABuilder,
        NFABuilderContext,
        buildNFA,
        epsilonClosed,
        newStateNum,
        epsilonTrans,
        condTrans,
        accept,
        initial,
    ) where

import           Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Data.CharSet  as CharSet
import qualified Language.Lexer.Tlex.Data.Graph    as Graph
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Syntax        as Tlex


data NFA a = NFA
    { nfaInitials :: [(MState.StateNum, Tlex.StartState)]
    , nfaTrans    :: MState.StateArray (NFAState a)
    }

-- |
--
-- TODO:
-- * support polymorphic trans condition; only support charset now.
--
data NFAState a = NState
    { nstAccepts      :: [Tlex.Accept a]
    , nstEpsilonTrans :: [MState.StateNum]
    , nstTrans        :: [(CharSet.CharSet, MState.StateNum)]
    }

epsilonClosed :: NFA a -> NFA a
epsilonClosed nfa@NFA{ nfaTrans } = nfa
    { nfaTrans = MState.mapArrayWithIx go nfaTrans
    }
    where
        go v s = s
            { nstEpsilonTrans = gr `MState.indexGraph` v
            }

        gr = MState.liftGraphOp Graph.transClosure
            do MState.stateArrayToGraph do fmap nstEpsilonTrans nfaTrans


data NFABuilderContext m = NFABuilderContext
    { nfaBCtxInitials     :: [(MState.StateNum, Tlex.StartState)]
    , nfaBCtxNextStateNum :: MState.StateNum
    , nfaBCtxStateMap     :: MState.StateMap (NFAState m)
    }

type NFABuilder m = State (NFABuilderContext m)

buildNFA :: NFABuilder m () -> NFA m
buildNFA builder =
    let bctx = execState builder initialBCtx
        arr = MState.totalStateMapToArray
            do nfaBCtxNextStateNum bctx
            do nfaBCtxStateMap bctx
    in epsilonClosed
        do NFA
            { nfaInitials = nfaBCtxInitials bctx
            , nfaTrans = arr
            }
    where
        initialBCtx = NFABuilderContext
            { nfaBCtxInitials = []
            , nfaBCtxNextStateNum = MState.initialStateNum
            , nfaBCtxStateMap = MState.emptyMap
            }

newStateNum :: NFABuilder m MState.StateNum
newStateNum = do
    ctx0 <- get
    let nextStateNum = nfaBCtxNextStateNum ctx0
    put do ctx0
            { nfaBCtxNextStateNum = succ nextStateNum
            }
    pure nextStateNum

epsilonTrans :: MState.StateNum -> MState.StateNum -> NFABuilder m ()
epsilonTrans sf st
    | sf == st  = pure ()
    | otherwise = modify' \ctx0@NFABuilderContext{ nfaBCtxStateMap } -> ctx0
        { nfaBCtxStateMap = addEpsTrans nfaBCtxStateMap
        }
    where
        addEpsTrans n = MState.insertOrUpdateMap sf
            do NState
                { nstAccepts = []
                , nstEpsilonTrans = [st]
                , nstTrans = []
                }
            do \s@NState{ nstEpsilonTrans } -> s
                { nstEpsilonTrans = st:nstEpsilonTrans
                }
            do n

condTrans :: MState.StateNum -> CharSet.CharSet -> MState.StateNum -> NFABuilder m ()
condTrans sf r st = modify' \ctx0@NFABuilderContext{ nfaBCtxStateMap } -> ctx0
    { nfaBCtxStateMap = addCondTrans nfaBCtxStateMap
    }
    where
        addCondTrans n = MState.insertOrUpdateMap sf
            do NState
                { nstAccepts = []
                , nstEpsilonTrans = []
                , nstTrans = [(r, st)]
                }
            do \s@NState{ nstTrans } -> s
                { nstTrans = (r, st):nstTrans
                }
            do n

accept :: MState.StateNum -> Tlex.Accept m -> NFABuilder m ()
accept s x = modify' \ctx0@NFABuilderContext{ nfaBCtxStateMap } -> ctx0
    { nfaBCtxStateMap = addAccept nfaBCtxStateMap
    }
    where
        addAccept n = MState.insertOrUpdateMap s
            do NState
                { nstAccepts = [x]
                , nstEpsilonTrans = []
                , nstTrans = []
                }
            do \ns@NState{ nstAccepts } -> ns
                { nstAccepts = x:nstAccepts
                }
            do n

initial :: MState.StateNum -> Tlex.StartState -> NFABuilder m ()
initial s x = modify' \ctx0@NFABuilderContext{ nfaBCtxInitials } -> ctx0
    { nfaBCtxInitials = (s, x):nfaBCtxInitials
    }
