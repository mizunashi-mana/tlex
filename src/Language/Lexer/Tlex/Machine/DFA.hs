module Language.Lexer.Tlex.Machine.DFA (
    DFA (..),
    DFAState (..),
    DFABuilder,
    DFABuilderContext,
    buildDFA,
    newStateNum,
    insertTrans,
    accept,
    initial,
) where

import Language.Lexer.Tlex.Prelude

import qualified Data.Hashable                     as Hashable
import qualified Data.HashMap.Strict               as HashMap
import qualified Language.Lexer.Tlex.Data.EnumMap  as EnumMap
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
    , dstTrans :: EnumMap.EnumMap Char MState.StateNum
    , dstOtherTrans :: Maybe MState.StateNum
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
            , dfaBCtxStateMap = MState.emptyMap
            }

newStateNum :: DFABuilder s m MState.StateNum
newStateNum = do
    ctx0 <- get
    let nextStateNum = dfaBCtxNextStateNum ctx0
    put $ ctx0
        { dfaBCtxNextStateNum = succ nextStateNum
        }
    pure nextStateNum

insertTrans :: MState.StateNum -> DFAState s m -> DFABuilder s m ()
insertTrans sf st = modify' \ctx0@DFABuilderContext{ dfaBCtxStateMap } -> ctx0
    { dfaBCtxStateMap = addCondTrans dfaBCtxStateMap
    }
    where
        addCondTrans n = MState.insertMap sf st n

accept :: MState.StateNum -> Tlex.Accept s m -> DFABuilder s m ()
accept s x = modify' \ctx0@DFABuilderContext{ dfaBCtxStateMap } -> ctx0
    { dfaBCtxStateMap = addAccept dfaBCtxStateMap
    }
    where
        addAccept n = MState.insertOrUpdateMap s
            do DState
                { dstAccepts = [x]
                , dstTrans = EnumMap.empty
                , dstOtherTrans = Nothing
                }
            do \ds@DState { dstAccepts } -> ds
                { dstAccepts = x:dstAccepts
                }
            do n

initial :: Eq s => Hashable.Hashable s => MState.StateNum -> s -> DFABuilder s m ()
initial s x = modify' \ctx0@DFABuilderContext{ dfaBCtxInitials } -> ctx0
    { dfaBCtxInitials = HashMap.insert x s dfaBCtxInitials
    }
