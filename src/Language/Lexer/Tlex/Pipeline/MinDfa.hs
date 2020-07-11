module Language.Lexer.Tlex.Pipeline.MinDfa (
    minDfa,
) where

import Language.Lexer.Tlex.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Language.Lexer.Tlex.Machine.DFA   as DFA
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Data.EnumMap as EnumMap
import qualified Language.Lexer.Tlex.Syntax as Tlex


minDfa :: DFA.DFA a -> DFA.DFA a
minDfa dfa = undefined p where
    p = buildPartition dfa


data Partition = Partition
    { partitionMap :: MState.StateMap MState.StateNum
    , partitionMember :: MState.StateMap MState.StateSet
    }

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
                let (p1, q1) = go2 a p0 q0
                in go p1 q1

        go2 a p0 q0 = foldl'
            do \(p, q) x -> go3 p q x
            do (p0, q0)
            do [ x | (_, x) <- EnumMap.assocs do findIncomingTrans a ]

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
            do \m s -> case MState.lookupMap s rtrans of
                Nothing -> m
                Just ms -> EnumMap.unionWith MState.unionSet m ms
            do EnumMap.empty
            do MState.setToList ss

        rtrans = revTrans dfa

acceptGroup :: DFA.DFA a -> HashMap.HashMap (Maybe (Tlex.Accept a)) MState.StateSet
acceptGroup DFA.DFA{ dfaTrans } = foldl'
    do \m (s, dst) -> case DFA.dstAccepts dst of
        []    -> insertState Nothing s m
        acc:_ -> insertState (Just acc) s m
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

revTrans :: DFA.DFA a -> MState.StateMap (EnumMap.EnumMap Char MState.StateSet)
revTrans DFA.DFA{ dfaTrans } = foldl'
    do \m0 (sf, dst) -> foldl'
        do \m (c, st) -> insertTrans sf c st m
        do m0
        do EnumMap.assocs do DFA.dstTrans dst
    do MState.emptyMap
    do MState.arrayAssocs dfaTrans
    where
        insertTrans sf c st m = MState.insertOrUpdateMap st
            do EnumMap.singleton c do MState.singletonSet sf
            do \cm -> EnumMap.insertOrUpdate c
                do MState.singletonSet sf
                do \ss -> MState.insertSet sf ss
                do cm
            do m
