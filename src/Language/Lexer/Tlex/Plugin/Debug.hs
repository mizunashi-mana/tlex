module Language.Lexer.Tlex.Plugin.Debug (
    outputDfaToDot,
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Plugin.Debug.Graphviz as Graphviz
import qualified Language.Lexer.Tlex.Machine.DFA   as DFA
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Data.IntMap as IntMap
import qualified Language.Lexer.Tlex.Data.EnumMap as EnumMap


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
            do case DFA.dstOtherTrans dst of
                Nothing -> []
                Just ot -> [edge sn -1 ot]
            ++
            [ edge sn lb to
            | (lb, to) <- IntMap.assocs do DFA.dstTrans dst
            ]

        edge :: MState.StateNum -> Int -> MState.StateNum -> Graphviz.Edge
        edge fr lb to = Graphviz.Edge
            { Graphviz.edgeFrom = show do fromEnum fr
            , Graphviz.edgeTo = show do fromEnum to
            , Graphviz.edgeLabel = Just do show lb
            }
