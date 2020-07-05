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

import           Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Data.EnumMap  as EnumMap
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Syntax        as Tlex


data DFA a = DFA
    { dfaInitials :: EnumMap.EnumMap Tlex.StartState MState.StateNum
    , dfaTrans    :: MState.StateArray (DFAState a)
    }
    deriving (Eq, Show, Functor)

-- |
--
-- TODO:
-- * support byte map transition.
--
data DFAState a = DState
    { dstAccepts    :: [Tlex.Accept a]
    , dstTrans      :: EnumMap.EnumMap Char MState.StateNum
    , dstOtherTrans :: Maybe MState.StateNum
    }
    deriving (Eq, Show, Functor)


data DFABuilderContext m = DFABuilderContext
    { dfaBCtxInitials     :: EnumMap.EnumMap Tlex.StartState MState.StateNum
    , dfaBCtxNextStateNum :: MState.StateNum
    , dfaBCtxStateMap     :: MState.StateMap (DFAState m)
    }

type DFABuilder m = State (DFABuilderContext m)

buildDFA :: DFABuilder m () -> DFA m
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
            { dfaBCtxInitials = EnumMap.empty
            , dfaBCtxNextStateNum = MState.initialStateNum
            , dfaBCtxStateMap = MState.emptyMap
            }

newStateNum :: DFABuilder m MState.StateNum
newStateNum = do
    ctx0 <- get
    let nextStateNum = dfaBCtxNextStateNum ctx0
    put do ctx0
            { dfaBCtxNextStateNum = succ nextStateNum
            }
    pure nextStateNum

insertTrans :: MState.StateNum -> DFAState m -> DFABuilder m ()
insertTrans sf st = modify' \ctx0@DFABuilderContext{ dfaBCtxStateMap } -> ctx0
    { dfaBCtxStateMap = addCondTrans dfaBCtxStateMap
    }
    where
        addCondTrans n = MState.insertMap sf st n

accept :: MState.StateNum -> Tlex.Accept m -> DFABuilder m ()
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

initial :: MState.StateNum -> Tlex.StartState -> DFABuilder m ()
initial s x = modify' \ctx0@DFABuilderContext{ dfaBCtxInitials } -> ctx0
    { dfaBCtxInitials = EnumMap.insert x s dfaBCtxInitials
    }
