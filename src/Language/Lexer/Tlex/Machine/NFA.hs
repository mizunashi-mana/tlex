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

import Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Syntax as Tlex
import qualified Language.Lexer.Tlex.Data.CharSet as CharSet
import qualified Language.Lexer.Tlex.Data.Graph as Graph
import qualified Language.Lexer.Tlex.Machine.State as MState


data NFA s a = NFA
    { nfaInitials :: [(MState.StateNum, s)]
    , nfaTrans :: MState.StateArray (NFAState s a)
    }

-- |
--
-- TODO:
-- * support polymorphic trans condition; only support charset now.
--
data NFAState s a = NState
    { nstAccepts :: [Tlex.Accept s a]
    , nstEpsilonTrans :: [MState.StateNum]
    , nstTrans :: [(CharSet.CharSet, MState.StateNum)]
    }

epsilonClosed :: NFA s a -> NFA s a
epsilonClosed nfa@NFA{ nfaTrans } = nfa
    { nfaTrans = MState.mapArrayWithIx go nfaTrans
    }
    where
        go v s = s
            { nstEpsilonTrans = gr `MState.indexGraph` v
            }

        gr = MState.liftGraphOp Graph.transClosure
            do MState.stateArrayToGraph do fmap nstEpsilonTrans nfaTrans


data NFABuilderContext s m = NFABuilderContext
    { nfaBCtxInitials :: [(MState.StateNum, s)]
    , nfaBCtxNextStateNum :: MState.StateNum
    , nfaBCtxStateMap :: MState.StateMap (NFAState s m)
    }

type NFABuilder s m = State (NFABuilderContext s m)

buildNFA :: NFABuilder s m () -> NFA s m
buildNFA builder =
    let bctx = execState builder initialBCtx
        arr = MState.totalStateMapToArray
            do nfaBCtxNextStateNum bctx
            do nfaBCtxStateMap bctx
    in epsilonClosed $ NFA
        { nfaInitials = nfaBCtxInitials bctx
        , nfaTrans = arr
        }
    where
        initialBCtx = NFABuilderContext
            { nfaBCtxInitials = []
            , nfaBCtxNextStateNum = MState.initialStateNum
            , nfaBCtxStateMap = MState.emptyMap
            }

newStateNum :: NFABuilder s m MState.StateNum
newStateNum = do
    ctx0 <- get
    let nextStateNum = nfaBCtxNextStateNum ctx0
    put $ ctx0
        { nfaBCtxNextStateNum = succ nextStateNum
        }
    pure nextStateNum

epsilonTrans :: MState.StateNum -> MState.StateNum -> NFABuilder s m ()
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

condTrans
    :: MState.StateNum -> CharSet.CharSet -> MState.StateNum -> NFABuilder s m ()
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

accept :: MState.StateNum -> Tlex.Accept s m -> NFABuilder s m ()
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

initial :: MState.StateNum -> s -> NFABuilder s m ()
initial s x = modify' \ctx0@NFABuilderContext{ nfaBCtxInitials } -> ctx0
    { nfaBCtxInitials = (s, x):nfaBCtxInitials
    }
