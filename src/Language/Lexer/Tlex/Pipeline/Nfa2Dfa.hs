module Language.Lexer.Tlex.Pipeline.Nfa2Dfa (
    nfa2Dfa,
) where

import Language.Lexer.Tlex.Prelude

import qualified Data.Hashable as Hashable
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Language.Lexer.Tlex.Data.CharMap as CharMap
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Machine.NFA as NFA
import qualified Language.Lexer.Tlex.Machine.DFA as DFA


nfa2Dfa :: Eq s => Hashable.Hashable s => NFA.NFA s a -> DFA.DFA s a
nfa2Dfa nfa = DFA.buildDFA $ modify' \dfaBuilderCtx0 -> nfa2DfaCtxDFABuilderCtx $ execState
    do nfa2DfaM nfa
    do Nfa2DfaContext
        { nfa2DfaCtxStateMap = HashMap.empty
        , nfa2DfaCtxDFABuilderCtx = dfaBuilderCtx0
        }


data Nfa2DfaContext s m = Nfa2DfaContext
    { nfa2DfaCtxStateMap :: HashMap.HashMap MState.StateSet MState.StateNum
    , nfa2DfaCtxDFABuilderCtx :: DFA.DFABuilderContext s m
    }

type Nfa2DfaM s m = State (Nfa2DfaContext s m)

liftBuilderOp :: DFA.DFABuilder s m a -> Nfa2DfaM s m a
liftBuilderOp builder = do
    ctx0 <- get
    let (x, builderCtx1) = runState builder do nfa2DfaCtxDFABuilderCtx ctx0
    put $ ctx0
        { nfa2DfaCtxDFABuilderCtx = builderCtx1
        }
    pure x

registerNewState :: Eq s => Hashable.Hashable s => MState.StateSet -> Nfa2DfaM s m MState.StateNum
registerNewState nfaSs = do
    dfaSn <- liftBuilderOp DFA.newStateNum
    modify' \ctx0@Nfa2DfaContext{ nfa2DfaCtxStateMap } -> ctx0
        { nfa2DfaCtxStateMap = HashMap.insert nfaSs dfaSn nfa2DfaCtxStateMap
        }
    pure dfaSn

nfa2DfaM :: Eq s => Hashable.Hashable s => NFA.NFA s m -> Nfa2DfaM s m ()
nfa2DfaM NFA.NFA{ nfaInitials, nfaTrans } = do
    initials <- forM nfaInitials \(nfaSn, s) -> do
        let nfaSs = MState.singletonSet nfaSn
        dfaSn <- registerNewState nfaSs
        liftBuilderOp do DFA.initial dfaSn s
        pure (dfaSn, nfaSs)

    buildStateMap initials
    where
        buildStateMap = \case
            []                   -> pure ()
            (dfaSn, nfaSs):rest0 -> do
                (rest1, dst) <- buildDFAState dfaSn nfaSs rest0
                liftBuilderOp do DFA.insertTrans dfaSn dst
                buildStateMap rest1

        buildDFAState dfaSn nfaSs rest0 = do
            (rest1, accs1, trans1) <- foldM
                do \(rest, accs, trans) nfaSn -> do
                    let nfaState = nfaTrans `MState.indexArray` nfaSn
                    -- TODO
                    pure (rest, undefined accs dfaSn, undefined trans nfaState)
                do (rest0, HashSet.empty, CharMap.empty)
                do MState.stateSetToList nfaSs
            pure
                ( rest1
                , DFA.DState
                    { dstAccepts = HashSet.toList accs1
                    , dstTrans = trans1
                    }
                )
