module Language.Lexer.Tlex.Data.Graph (
    transClosure,
) where

import qualified Data.Array    as Array
import           Data.Foldable
import qualified Data.Graph    as Graph

transClosure :: Graph.Graph -> Graph.Graph
transClosure gr = Array.listArray r [ goDfs v | v <- Graph.vertices gr ] where
    r = Array.bounds gr

    goDfs v = foldMap (\t -> toList t) do Graph.dfs gr [v]
