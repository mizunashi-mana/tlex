{-# LANGUAGE TemplateHaskell #-}

module Language.Lexer.Tlex.Data.TypeableTH (
    liftTypeFromTypeable,
    tyConToType,
) where

import           Prelude

import           Data.Foldable       (foldl')
import qualified Data.Typeable       as Typeable
import qualified Language.Haskell.TH as TH

liftTypeFromTypeable :: Typeable.Typeable a => Typeable.Proxy a -> TH.Q TH.Type
liftTypeFromTypeable p = go do Typeable.typeRep p where
    go r0 =
        let (tyCon, rs) = Typeable.splitTyConApp r0
        in foldl'
            do \tq1 tq2 -> [t|$(tq1) $(tq2)|]
            do tyConToType tyCon
            do [ go r | r <- rs ]

-- |
--
-- TODO: correct reifying
-- NOTICE: introduce @reifyType@ by GHC 8.10
--
tyConToType :: Typeable.TyCon -> TH.Q TH.Type
tyConToType tyCon = do
    mn <- TH.lookupTypeName tyConQualifiedName
    case mn of
        Just n  -> pure do TH.ConT n
        Nothing -> case tyConQualifiedName of
            "GHC.Tuple.()" ->
                pure do TH.TupleT 0
            _  ->
                fail do "Missing type: " ++ tyConQualifiedName
    where
        tyConQualifiedName = Typeable.tyConModule tyCon ++ "." ++ Typeable.tyConName tyCon
