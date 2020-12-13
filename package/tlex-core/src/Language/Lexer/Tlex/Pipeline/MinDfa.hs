module Language.Lexer.Tlex.Pipeline.MinDfa (
    minDfa,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.HashMap.Strict                 as HashMap
import qualified Data.HashSet                        as HashSet
import qualified Data.IntMap.Strict                  as IntMap
import qualified Language.Lexer.Tlex.Data.EnumMap    as EnumMap
import qualified Language.Lexer.Tlex.Machine.DFA     as DFA
import qualified Language.Lexer.Tlex.Machine.Pattern as Pattern
import qualified Language.Lexer.Tlex.Machine.State   as MState


minDfa :: DFA.DFA a -> DFA.DFA a
minDfa dfa = DFA.buildDFA
    do modify' \dfaBuilderCtx0 -> minDfaCtxDFABuilderCtx
        do execState
            do minDfaM dfa
            do MinDfaContext
                { minDfaCtxStateMap = MState.emptyMap
                , minDfaCtxDFABuilderCtx = dfaBuilderCtx0
                }


data MinDfaContext m = MinDfaContext
    { minDfaCtxStateMap      :: MState.StateMap MState.StateNum
    , minDfaCtxDFABuilderCtx :: DFA.DFABuilderContext m
    }
    deriving (Eq, Show, Functor)

type MinDfaM m = State (MinDfaContext m)

liftBuilderOp :: DFA.DFABuilder m a -> MinDfaM m a
liftBuilderOp builder = do
    ctx0 <- get
    let (x, builderCtx1) = runState builder do minDfaCtxDFABuilderCtx ctx0
    put do ctx0
            { minDfaCtxDFABuilderCtx = builderCtx1
            }
    pure x

registerNewState :: MState.StateNum -> MinDfaM m MState.StateNum
registerNewState r = do
    sn <- liftBuilderOp DFA.newStateNum
    modify' \ctx0@MinDfaContext{ minDfaCtxStateMap } -> ctx0
        { minDfaCtxStateMap = MState.insertMap r sn minDfaCtxStateMap
        }
    pure sn

getOrRegisterState :: MState.StateNum -> MinDfaM m MState.StateNum
getOrRegisterState r = do
    ctx0 <- get
    case MState.lookupMap r do minDfaCtxStateMap ctx0 of
        Just sn -> pure sn
        Nothing -> registerNewState r

minDfaM :: DFA.DFA a -> MinDfaM a ()
minDfaM dfa@DFA.DFA{ dfaTrans } = do
    forM_
        do EnumMap.assocs do DFA.dfaInitials dfa
        do \(startS, sn) -> do
            newSn <- getOrRegisterStateByOldState sn
            liftBuilderOp do DFA.initial newSn startS

    forM_
        do MState.assocsMap do partitionMember p
        do \(r, ss) -> do
            newSn <- getOrRegisterState r
            newDst <- buildDFAState ss
            liftBuilderOp do DFA.insertTrans newSn newDst
    where
        p = buildPartition dfa

        getOrRegisterStateByOldState oldSn =
            let r = case MState.lookupMap oldSn do partitionMap p of
                    Nothing -> error "unreachable"
                    Just s  -> s
            in getOrRegisterState r

        buildDFAState ss = buildDst do
            forM_
                do MState.setToList ss
                do \s -> do
                    let dst = MState.indexArray dfaTrans s
                    forM_
                        do DFA.dstAccepts dst
                        do \acc -> insertAcceptToDst acc

                    forM_
                        do IntMap.assocs do DFA.dstTrans dst
                        do \(c, sn) -> do
                            ctx0 <- get
                            case IntMap.lookup c do dstBuilderCtxTrans ctx0 of
                                Just{}  -> pure ()
                                Nothing -> do
                                    newSn <- liftMinDfaOp do getOrRegisterStateByOldState sn
                                    modify' \ctx -> ctx
                                        { dstBuilderCtxTrans = IntMap.insert c newSn
                                            do dstBuilderCtxTrans ctx
                                        }

                    case DFA.dstOtherTrans dst of
                        Nothing -> pure ()
                        Just sn -> do
                            ctx <- get
                            case dstBuilderCtxOtherTrans ctx of
                                Just{}  -> pure ()
                                Nothing -> do
                                    newSn <- liftMinDfaOp do getOrRegisterStateByOldState sn
                                    put do ctx
                                            { dstBuilderCtxOtherTrans = Just newSn
                                            }

data DFAStateBuilderContext a = DStateBuilderContext
    { dstBuilderCtxAccepts    :: EnumMap.EnumMap Pattern.AcceptPriority (Pattern.Accept a)
    , dstBuilderCtxTrans      :: IntMap.IntMap MState.StateNum
    , dstBuilderCtxOtherTrans :: Maybe MState.StateNum
    , dstBuilderCtxMinDfaCtx  :: MinDfaContext a
    }
    deriving (Eq, Show, Functor)

type DFAStateBuilder a = State (DFAStateBuilderContext a)

buildDst :: DFAStateBuilder a () -> MinDfaM a (DFA.DFAState a)
buildDst builder = do
    minDfaCtx0 <- get
    let ctx = execState builder do
            DStateBuilderContext
                { dstBuilderCtxAccepts = EnumMap.empty
                , dstBuilderCtxTrans = IntMap.empty
                , dstBuilderCtxOtherTrans = Nothing
                , dstBuilderCtxMinDfaCtx = minDfaCtx0
                }
    put do dstBuilderCtxMinDfaCtx ctx
    pure DFA.DState
        { DFA.dstAccepts = [ acc | (_, acc) <- EnumMap.toDescList do dstBuilderCtxAccepts ctx ]
        , DFA.dstTrans   = dstBuilderCtxTrans ctx
        , DFA.dstOtherTrans = dstBuilderCtxOtherTrans ctx
        }

liftMinDfaOp :: MinDfaM m a -> DFAStateBuilder m a
liftMinDfaOp builder = do
    ctx0 <- get
    let (x, builderCtx1) = runState builder do dstBuilderCtxMinDfaCtx ctx0
    put do ctx0
            { dstBuilderCtxMinDfaCtx = builderCtx1
            }
    pure x

insertAcceptToDst :: Pattern.Accept a -> DFAStateBuilder a ()
insertAcceptToDst acc = modify' \builder -> builder
    { dstBuilderCtxAccepts = EnumMap.insert
        do Pattern.accPriority acc
        do acc
        do dstBuilderCtxAccepts builder
    }


data Partition = Partition
    { partitionMap    :: MState.StateMap MState.StateNum
    , partitionMember :: MState.StateMap MState.StateSet
    }
    deriving (Eq, Show)

emptyPartition :: Partition
emptyPartition = Partition
    { partitionMap = MState.emptyMap
    , partitionMember = MState.emptyMap
    }

insertToPartition :: MState.StateSet -> Partition -> Partition
insertToPartition ss p0 = case MState.setToList ss of
    []   -> p0
    s0:_ -> Partition
        { partitionMap = foldl'
            do \m s -> MState.insertMap s s0 m
            do partitionMap p0
            do MState.setToList ss
        , partitionMember = MState.insertMap s0 ss
            do partitionMember p0
        }

buildPartition :: DFA.DFA a -> Partition
buildPartition dfa =
    let (p0, q0) = foldl'
            do \(p, q) (k, ss) ->
                ( insertToPartition ss p
                , case k of
                    Nothing -> q
                    Just{}  -> HashSet.insert ss q
                )
            do (emptyPartition, HashSet.empty)
            do HashMap.toList do acceptGroup dfa
    in go p0 q0
    where
        go p0 q0 = case HashSet.toList q0 of
            []  -> p0
            a:_ ->
                let (p1, q1) = go2 a p0 do
                        HashSet.delete a q0
                in go p1 q1

        go2 a p0 q0 = foldl'
            do \(p, q) x -> go3 p q x
            do (p0, q0)
            let rt = findIncomingTrans a
            in HashSet.toList do
                HashSet.fromList
                    [ x
                    | x <- dfaRevTransOther rt:
                        [ x | (_, x) <- IntMap.assocs do dfaRevTrans rt ]
                    , not do MState.nullSet x
                    ]

        go3 p0 q0 x = foldl'
            do \(p, q) (sp, xy) ->
                let y = case MState.lookupMap sp do partitionMember p0 of
                        Nothing -> error "unreachable"
                        Just ss -> ss
                    lengthY = MState.lengthSet y
                    lengthXY = MState.lengthSet xy
                in if
                    | lengthY == lengthXY ->
                        (p, q)
                    | otherwise ->
                        let diffYX = MState.diffSet y xy
                            splitY s1 s2 = case MState.setToList s2 of
                                []    -> error "unreachable"
                                sp2:_ -> Partition
                                    { partitionMap = foldl'
                                        do \m s -> MState.insertMap s sp2 m
                                        do partitionMap p
                                        do MState.setToList s2
                                    , partitionMember = partitionMember p
                                        & MState.insertMap sp s1
                                        & MState.insertMap sp2 s2
                                    }
                            p' = case MState.memberSet sp xy of
                                True  -> splitY xy diffYX
                                False -> splitY diffYX xy
                            q' = case HashSet.member y q of
                                True -> HashSet.delete y q
                                    & HashSet.insert xy
                                    & HashSet.insert diffYX
                                False ->
                                    let y' = case lengthXY <= lengthY `div` 2 of
                                            True  -> xy
                                            False -> diffYX
                                    in HashSet.insert y' q
                        in (p', q')
            do (p0, q0)
            do MState.assocsMap do findY p0 x

        findY Partition{ partitionMap } x = foldl'
            do \ym s -> case MState.lookupMap s partitionMap of
                Nothing -> error "unreachable"
                Just sp -> MState.insertOrUpdateMap sp
                    do MState.singletonSet s
                    do \ss -> MState.insertSet s ss
                    do ym
            do MState.emptyMap
            do MState.setToList x

        findIncomingTrans ss = foldl'
            do \rt0 s -> case MState.lookupMap s rtrans of
                Nothing -> rt0
                Just rt -> DFARevTrans
                    { dfaRevTrans = IntMap.mergeWithKey
                        do \_ ss1 ss2 -> Just do MState.unionSet ss1 ss2
                        do \t1 -> t1 <&> \ss1 -> MState.unionSet ss1
                            do dfaRevTransOther rt
                        do \t2 -> t2 <&> \ss2 -> MState.unionSet ss2
                            do dfaRevTransOther rt0
                        do dfaRevTrans rt0
                        do dfaRevTrans rt
                    , dfaRevTransOther = MState.unionSet
                        do dfaRevTransOther rt0
                        do dfaRevTransOther rt
                    }
            do DFARevTrans
                { dfaRevTrans = IntMap.empty
                , dfaRevTransOther = MState.emptySet
                }
            do MState.setToList ss

        rtrans = revTrans dfa

acceptGroup :: DFA.DFA a -> HashMap.HashMap (Maybe Pattern.AcceptPriority) MState.StateSet
acceptGroup DFA.DFA{ dfaTrans } = foldl'
    do \m (s, dst) -> case DFA.dstAccepts dst of
        []    -> insertState Nothing s m
        acc:_ -> insertState
            do Just do Pattern.accPriority acc
            do s
            do m
    do HashMap.empty
    do MState.arrayAssocs dfaTrans
    where
        insertState k s m = case HashMap.lookup k m of
            Nothing -> HashMap.insert k
                do MState.singletonSet s
                do m
            Just ss -> HashMap.insert k
                do MState.insertSet s ss
                do m


data DFARevTrans a = DFARevTrans
    { dfaRevTrans      :: IntMap.IntMap MState.StateSet
    , dfaRevTransOther :: MState.StateSet
    }

revTrans :: DFA.DFA a -> MState.StateMap (DFARevTrans a)
revTrans DFA.DFA{ dfaTrans } = foldl'
    do \m0 (sf, dst) ->
        let trans = DFA.dstTrans dst
            m1 = foldl'
                do \m (c, st) -> insertTrans sf c st m
                do m0
                do IntMap.assocs trans
        in case DFA.dstOtherTrans dst of
            Nothing -> m1
            Just st -> insertOtherTrans sf st trans m1
    do MState.emptyMap
    do MState.arrayAssocs dfaTrans
    where
        insertTrans sf c st m0 = MState.insertOrUpdateMap st
            do DFARevTrans
                { dfaRevTrans = IntMap.singleton c do MState.singletonSet sf
                , dfaRevTransOther = MState.emptySet
                }
            do \rtrans ->
                let rtransRevTrans = dfaRevTrans rtrans
                in rtrans
                    { dfaRevTrans = case IntMap.lookup c rtransRevTrans of
                        Nothing -> IntMap.insert c
                            do MState.insertSet sf do dfaRevTransOther rtrans
                            do rtransRevTrans
                        Just ss -> IntMap.insert c
                            do MState.insertSet sf ss
                            do rtransRevTrans
                    }
            do m0

        insertOtherTrans sf st trans m0 = MState.insertOrUpdateMap st
            do DFARevTrans
                { dfaRevTrans = trans <&> \_ -> MState.emptySet
                , dfaRevTransOther = MState.singletonSet sf
                }
            do \rtrans -> DFARevTrans
                { dfaRevTrans = IntMap.mergeWithKey
                    do \_ ss _ -> Just ss
                    do \rt -> rt <&> \ss -> MState.insertSet sf ss
                    do \t -> t <&> \_ -> dfaRevTransOther rtrans
                    do dfaRevTrans rtrans
                    do trans
                , dfaRevTransOther = MState.insertSet sf
                    do dfaRevTransOther rtrans
                }
            do m0
