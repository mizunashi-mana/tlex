module Language.Lexer.Tlex.Plugin.Debug (
    outputDfaToDot,
    Graphviz.outputAst,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.IntSet                               as IntSet
import qualified Data.IntMap.Strict                        as IntMap
import qualified Data.HashMap.Strict                       as HashMap
import qualified Language.Lexer.Tlex.Data.EnumMap          as EnumMap
import qualified Language.Lexer.Tlex.Machine.DFA           as DFA
import qualified Language.Lexer.Tlex.Machine.State         as MState
import qualified Language.Lexer.Tlex.Plugin.Debug.Graphviz as Graphviz
import qualified Prelude


newtype EdgeBuilder = EdgeBuilder
    { unEdgeBuilder :: HashMap.HashMap
        (MState.StateNum, MState.StateNum)
        IntSet.IntSet
    }

instance Semigroup EdgeBuilder where
    EdgeBuilder m1 <> EdgeBuilder m2 = EdgeBuilder do HashMap.unionWith (<>) m1 m2

instance Monoid EdgeBuilder where
    mempty = EdgeBuilder HashMap.empty

edgeBuilder :: MState.StateNum -> Int -> MState.StateNum -> EdgeBuilder
edgeBuilder sf lb st = EdgeBuilder do HashMap.singleton (sf, st) do IntSet.singleton lb

outputDfaToDot :: DFA.DFA a -> Graphviz.Ast
outputDfaToDot dfa = Graphviz.Ast
    { Graphviz.nodes = initialNode :
        [ node sn dst
        | (sn, dst) <- MState.arrayAssocs do DFA.dfaTrans dfa
        ]
    , Graphviz.edges = initialEdges ++
        concatMap
            do \(sn, dst) -> edges sn dst
            do MState.arrayAssocs do DFA.dfaTrans dfa
    }
    where
        initialNode = Graphviz.Node
            { Graphviz.nodeId = "init"
            , Graphviz.nodeLabel = Nothing
            , Graphviz.nodeShape = Nothing
            }

        node sn dst = Graphviz.Node
            { Graphviz.nodeId = show do fromEnum sn
            , Graphviz.nodeLabel = Nothing
            , Graphviz.nodeShape = case DFA.dstAccepts dst of
                [] -> Nothing
                _  -> Just Graphviz.DoubleCircle
            }

        initialEdges =
            [ Graphviz.Edge
                { Graphviz.edgeFrom = "init"
                , Graphviz.edgeTo = show do fromEnum sn
                , Graphviz.edgeLabel = Nothing
                }
            | (_, sn) <- EnumMap.assocs do DFA.dfaInitials dfa
            ]

        edges sn dst =
            let builder =
                    do case DFA.dstOtherTrans dst of
                        Nothing -> mempty
                        Just ot -> edgeBuilder sn -1 ot
                    <> foldMap
                        do \(lb, to) -> edgeBuilder sn lb to
                        do IntMap.assocs do DFA.dstTrans dst
            in
                [ edge fr lb to
                | ((fr, to), lb) <- HashMap.toList do unEdgeBuilder builder
                ]

        edge :: MState.StateNum -> IntSet.IntSet -> MState.StateNum -> Graphviz.Edge
        edge fr lb to = Graphviz.Edge
            { Graphviz.edgeFrom = show do fromEnum fr
            , Graphviz.edgeTo = show do fromEnum to
            , Graphviz.edgeLabel = edgeLabel lb
            }

edgeLabel :: IntSet.IntSet -> Maybe Prelude.String
edgeLabel xs0 = case IntSet.toAscList xs0 of
    []    -> Nothing
    x0:xs ->
        let endPrevRange (s, p, b) = case b of
                True  -> s
                False -> s . ("-" ++) . (show p ++)
            s0 = endPrevRange
                do foldl'
                    do \ctx@(!s, !p, _) x -> if
                        | x == p + 1 -> (s, x, False)
                        | otherwise  ->
                            ( endPrevRange ctx . ("," ++) . (show x ++)
                            , x
                            , True
                            )
                    do ((show x0 ++), x0, True)
                    do xs
        in Just do "[" ++ s0 "]"

