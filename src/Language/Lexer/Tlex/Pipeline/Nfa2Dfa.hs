module Language.Lexer.Tlex.Pipeline.Nfa2Dfa (
    nfa2Dfa,
) where

import Language.Lexer.Tlex.Prelude

import qualified Data.Hashable as Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
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

getOrNewState :: Eq s => Hashable.Hashable s => MState.StateSet -> Nfa2DfaM s m MState.StateNum
getOrNewState nfaSs = do


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
            (accs1, trans1) <- foldM
                do \(accs, trans) nfaSn -> do
                    let nfaState = nfaTrans `MState.indexArray` nfaSn
                        accs' = foldl'
                            do \m acc -> IntMap.insert
                                do Tlex.accPriority acc
                                do acc
                                do m
                            do accs
                            do NFA.nstAccepts nfaState
                        trans' = foldl' insertTrans trans do NFA.nstTrans nfaState
                    in pure (accs', trans')
                do (IntMap.empty, CharMap.empty)
                do MState.stateSetToList nfaSs

            (rest1, trans2) <- foldM
                do \(rest, trans) (c, nfaSs) -> do
                    ctx0 <- get
                    let nfa2DfaCtxStateMap = nfa2DfaCtxStateMap ctx0
                    (rest', dfaSn) <- case HashMap.lookup nfaSs nfa2DfaCtxStateMap of
                        Just dfaSn -> pure (rest, dfaSn)
                        Nothing -> do
                            dfaSn <- registerNewState nfaSs
                            ((dfaSn, nfaSs):rest, dfaSn)
                    pure (rest', CharMap.insert c dfaSn trans)
                do (rest0, CharMap.empty)
                do CharMap.assocs trans1

            pure
                ( rest1
                , DFA.DState
                    { dstAccepts = [ acc | (_, acc) <- IntMap.toDescList accs1 ]
                    , dstTrans = trans2
                    }
                )

        insertTrans trans (r, sn) = undefined trans r sn
