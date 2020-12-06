module Language.Lexer.Tlex.Plugin.Debug.Graphviz (
    NodeShape (..),
    Node (..),
    Edge (..),
    Ast (..),
    outputAst,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Prelude


data NodeShape
    = DoubleCircle
    | Circle
    deriving (Eq, Show, Ord, Enum)

type NodeId = Prelude.String

data Node = Node
    { nodeId    :: NodeId
    , nodeLabel :: Maybe Prelude.String
    , nodeShape :: Maybe NodeShape
    }
    deriving (Eq, Show)

data Edge = Edge
    { edgeFrom  :: NodeId
    , edgeTo    :: NodeId
    , edgeLabel :: Maybe Prelude.String
    }
    deriving (Eq, Show)

data Ast = Ast
    { nodes :: [Node]
    , edges :: [Edge]
    }
    deriving (Eq, Show)

outputAst :: Ast -> Prelude.String
outputAst ast =
    "digraph {\n" ++
    nodeDef ++
    edgeDef ++
    "}"
    where
        nodeDef = concatMap
            do \n ->
                nodeId n ++
                " [" ++
                do case nodeLabel n of
                    Just lb -> "label = \"" ++ lb ++ "\","
                    Nothing -> ""
                ++
                do case nodeShape n of
                    Nothing -> ""
                    Just sh ->
                        "shape = " ++
                        do case sh of
                            DoubleCircle -> "doublecircle"
                            Circle       -> "circle"
                        ++
                        ","
                ++
                "];\n"
            do nodes ast

        edgeDef = concatMap
            do \e ->
                edgeFrom e ++
                " -> " ++
                edgeTo e ++
                " [" ++
                do case edgeLabel e of
                    Just lb -> "label = \"" ++ lb ++ "\","
                    Nothing -> ""
                ++
                "];\n"
            do edges ast
