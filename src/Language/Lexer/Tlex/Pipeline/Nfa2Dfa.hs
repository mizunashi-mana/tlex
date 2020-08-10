module Language.Lexer.Tlex.Pipeline.Nfa2Dfa (
    nfa2Dfa,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.HashMap.Strict               as HashMap
import qualified Data.IntMap.Strict                as IntMap
import qualified Language.Lexer.Tlex.Data.CharSet  as CharSet
import qualified Language.Lexer.Tlex.Data.EnumMap  as EnumMap
import qualified Language.Lexer.Tlex.Machine.DFA   as DFA
import qualified Language.Lexer.Tlex.Machine.NFA   as NFA
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Syntax        as Tlex


nfa2Dfa :: NFA.NFA a -> DFA.DFA a
nfa2Dfa nfa = DFA.buildDFA
    do modify' \dfaBuilderCtx0 -> nfa2DfaCtxDFABuilderCtx
        do execState
            do nfa2DfaM nfa
            do Nfa2DfaContext
                { nfa2DfaCtxStateMap = HashMap.empty
                , nfa2DfaCtxDFABuilderCtx = dfaBuilderCtx0
                }


data Nfa2DfaContext m = Nfa2DfaContext
    { nfa2DfaCtxStateMap      :: HashMap.HashMap MState.StateSet MState.StateNum
    , nfa2DfaCtxDFABuilderCtx :: DFA.DFABuilderContext m
    }

type Nfa2DfaM m = State (Nfa2DfaContext m)

liftBuilderOp :: DFA.DFABuilder m a -> Nfa2DfaM m a
liftBuilderOp builder = do
    ctx0 <- get
    let (x, builderCtx1) = runState builder do nfa2DfaCtxDFABuilderCtx ctx0
    put do ctx0
            { nfa2DfaCtxDFABuilderCtx = builderCtx1
            }
    pure x

registerNewState :: MState.StateSet -> Nfa2DfaM m MState.StateNum
registerNewState nfaSs = do
    dfaSn <- liftBuilderOp DFA.newStateNum
    modify' \ctx0@Nfa2DfaContext{ nfa2DfaCtxStateMap } -> ctx0
        { nfa2DfaCtxStateMap = HashMap.insert nfaSs dfaSn nfa2DfaCtxStateMap
        }
    pure dfaSn

nfa2DfaM :: NFA.NFA m -> Nfa2DfaM m ()
nfa2DfaM NFA.NFA{ nfaInitials, nfaTrans } = do
    initials <- forM nfaInitials \(nfaSn, s) -> do
        let nfaSs = buildNfaSs nfaSn
        dfaSn <- registerNewState nfaSs
        liftBuilderOp do DFA.initial dfaSn s
        pure (dfaSn, nfaSs)

    buildStateMap initials
    where
        buildNfaSs nfaSn =
            let nfaState = nfaTrans `MState.indexArray` nfaSn
            in MState.listToSet do NFA.nstEpsilonTrans nfaState

        insertNfaSn nfaSn0 nfaSs0 =
            let nfaState0 = nfaTrans `MState.indexArray` nfaSn0
            in foldl'
                do \nfaSs nfaSn -> MState.insertSet nfaSn nfaSs
                do nfaSs0
                do NFA.nstEpsilonTrans nfaState0

        buildStateMap = \case
            []                   -> pure ()
            (dfaSn, nfaSs):rest0 -> do
                (rest1, dst) <- buildDFAState nfaSs rest0
                liftBuilderOp do DFA.insertTrans dfaSn dst
                buildStateMap rest1

        buildDFAState nfaSs0 rest0 = do
            (accs1, trans1, otherTrans1) <- foldM
                do \(accs, trans, otherTrans) nfaSn ->
                    let nfaState = nfaTrans `MState.indexArray` nfaSn
                        accs' = foldl'
                                do \m acc -> EnumMap.insert
                                    do Tlex.accPriority acc
                                    do acc
                                    do m
                                do accs
                                do NFA.nstAccepts nfaState
                        (trans', otherTrans') = foldl' insertTrans (trans, otherTrans)
                                                    do NFA.nstTrans nfaState
                    in pure (accs', trans', otherTrans')
                do (EnumMap.empty, EnumMap.empty, MState.emptySet)
                do MState.setToList nfaSs0

            let getOrRegisterNfaSs nfaSs rest = do
                    ctx0 <- get
                    let stateMap = nfa2DfaCtxStateMap ctx0
                    case HashMap.lookup nfaSs stateMap of
                        Just dfaSn -> pure (rest, dfaSn)
                        Nothing -> do
                            dfaSn <- registerNewState nfaSs
                            pure ((dfaSn, nfaSs):rest, dfaSn)

            (rest1, trans2) <- foldM
                do \(rest, trans) (c, nfaSs) -> do
                    (rest', dfaSn) <- getOrRegisterNfaSs nfaSs rest
                    pure (rest', IntMap.insert (fromEnum c) dfaSn trans)
                do (rest0, IntMap.empty)
                do EnumMap.assocs trans1

            (rest2, otherTrans2) <- case MState.nullSet otherTrans1 of
                True  -> pure (rest1, Nothing)
                False -> do
                    (rest, dfaSn) <- getOrRegisterNfaSs otherTrans1 rest1
                    pure (rest, Just dfaSn)

            pure
                ( rest2
                , DFA.DState
                    { dstAccepts = [ acc | (_, acc) <- EnumMap.toDescList accs1 ]
                    , dstTrans = trans2
                    , dstOtherTrans = otherTrans2
                    }
                )

        insertTrans (trans0, otherTrans0) (r, nfaSn) = case CharSet.toElements r of
            CharSet.StraightChars cs ->
                let ~newTrans = insertNfaSn nfaSn otherTrans0
                    trans1 = foldl'
                        do \trans c -> EnumMap.insertOrUpdate c
                            do newTrans
                            do \ss -> insertNfaSn nfaSn ss
                            do trans
                        do trans0
                        do cs
                in (trans1, otherTrans0)
            CharSet.ComplementChars cs ->
                let (diffTrans1, trans1) = foldl'
                                            do \(diffTrans, trans) c ->
                                                ( EnumMap.delete c diffTrans
                                                , EnumMap.insertOrUpdate c
                                                    MState.emptySet
                                                    id
                                                    trans
                                                )
                                            do (trans0, trans0)
                                            do cs
                    trans2 = EnumMap.foldlWithKey'
                                do \trans c ss -> EnumMap.insert c
                                    do insertNfaSn nfaSn ss
                                    do trans
                                do trans1
                                do diffTrans1
                in (trans2, insertNfaSn nfaSn otherTrans0)
