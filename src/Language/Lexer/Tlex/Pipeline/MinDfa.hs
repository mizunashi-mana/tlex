module Language.Lexer.Tlex.Pipeline.MinDfa (

) where

import qualified Data.HashMap as HashMap
import qualified Language.Lexer.Tlex.Machine.DFA   as DFA
import qualified Language.Lexer.Tlex.Machine.State as MState


minDfa :: DFA.DFA a -> DFA.DFA a
minDfa = undefined


data Partition = Partition
    { partitionMap :: MState.StateMap MState.StateNum
    , partitionMember :: MState.StateMap MState.StateSet
    }

emptyPartition :: Partition
emptyPartition = Partition
    { partitionMap = MState.emptyMap
    , partitionMember = MState.emptyMap
    }

buildPartition :: DFA.DFA a -> Partition
buildPartition dfa =
    let (p0, q0) = foldl'
            do \(p, q) (k, ss) ->
                ( insertToPartion ss p
                , case k of
                    Nothing -> q
                    Just{}  -> ss:q
                )
            (emptyPartition, [])
            do HashMap.assocs do acceptGroup dfa
    in go p0 q0
    where
        go p = \case
            []   -> p
            ss:q -> undefined ss q

acceptGroup :: DFA.DFA a -> HashMap (Maybe (Accept a)) MState.StateSet
acceptGroup DFA.DFA{ dfaTrans } = foldl'
    do \m (s, dst) -> case DFA.dstAccepts dst of
        [] -> HashMap.
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

