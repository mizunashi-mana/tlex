module Language.Lexer.Tlex.Machine.DFA (
    DFA (..),
    DFAState (..),
    DFABuilder,
    DFABuilderContext,
    buildDFA,
    newStateNum,
    condTrans,
    accept,
    initial,
) where

import Language.Lexer.Tlex.Prelude

import qualified Data.Hashable                     as Hashable
import qualified Data.HashMap.Strict               as HashMap
import qualified Language.Lexer.Tlex.Data.CharSet  as CharSet
import qualified Language.Lexer.Tlex.Data.CharMap  as CharMap
import qualified Language.Lexer.Tlex.Syntax        as Tlex
import qualified Language.Lexer.Tlex.Machine.State as MState


data DFA s a = DFA
    { dfaInitials :: HashMap.HashMap s MState.StateNum
    , dfaTrans :: MState.StateArray (DFAState s a)
    }

-- |
--
-- TODO:
-- * support byte map transition.
--
data DFAState s a = DState
    { dstAccepts :: [Tlex.Accept s a]
    , dstTrans :: CharMap.CharMap MState.StateNum
    }


data DFABuilderContext s m = DFABuilderContext
    { dfaBCtxInitials :: HashMap.HashMap s MState.StateNum
    , dfaBCtxNextStateNum :: MState.StateNum
    , dfaBCtxStateMap :: MState.StateMap (DFAState s m)
    }

type DFABuilder s m = State (DFABuilderContext s m)

buildDFA :: DFABuilder s m () -> DFA s m
buildDFA builder =
    let bctx = execState builder initialBCtx
        arr = MState.totalStateMapToArray
            do dfaBCtxNextStateNum bctx
            do dfaBCtxStateMap bctx
    in DFA
        { dfaInitials = dfaBCtxInitials bctx
        , dfaTrans = arr
        }
    where
        initialBCtx = DFABuilderContext
            { dfaBCtxInitials = HashMap.empty
            , dfaBCtxNextStateNum = MState.initialStateNum
            , dfaBCtxStateMap = MState.emptyStateMap
            }

newStateNum :: DFABuilder s m MState.StateNum
newStateNum = do
    ctx0 <- get
    let nextStateNum = dfaBCtxNextStateNum ctx0
    put $ ctx0
        { dfaBCtxNextStateNum = succ nextStateNum
        }
    pure nextStateNum

condTrans
    :: MState.StateNum -> CharSet.CharSet -> MState.StateNum -> DFABuilder s m ()
condTrans sf r st = modify' \ctx0@DFABuilderContext{ dfaBCtxStateMap } -> ctx0
    { dfaBCtxStateMap = addCondTrans dfaBCtxStateMap
    }
    where
        addCondTrans n = MState.insertOrUpdateMap sf
            do DState
                { dstAccepts = []
                , dstTrans = insertCharSetMap CharMap.empty
                }
            do \s@DState{ dstTrans } -> s
                { dstTrans = insertCharSetMap dstTrans
                }
            do n

        insertCharSetMap m = foldl' insertCharMap m do CharSet.toList r

        insertCharMap m c = CharMap.insert c st m

accept :: MState.StateNum -> Tlex.Accept s m -> DFABuilder s m ()
accept s x = modify' \ctx0@DFABuilderContext{ dfaBCtxStateMap } -> ctx0
    { dfaBCtxStateMap = addAccept dfaBCtxStateMap
    }
    where
        addAccept n = MState.insertOrUpdateMap s
            do DState
                { dstAccepts = [x]
                , dstTrans = CharMap.empty
                }
            do \ds@DState { dstAccepts } -> ds
                { dstAccepts = x:dstAccepts
                }
            do n

initial :: Eq s => Hashable.Hashable s => MState.StateNum -> s -> DFABuilder s m ()
initial s x = modify' \ctx0@DFABuilderContext{ dfaBCtxInitials } -> ctx0
    { dfaBCtxInitials = HashMap.insert x s dfaBCtxInitials
    }
