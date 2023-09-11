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

tyConToType :: Typeable.TyCon -> TH.Q TH.Type
tyConToType tyCon = TH.lookupTypeName tyConQualifiedName >>= \case
    Just n  -> pure do TH.ConT n
    Nothing -> TH.lookupTypeName tyConName >>= \case
        Just n  -> pure do TH.ConT n
        Nothing -> case tyConName of
            "()" -> pure do TH.TupleT 0
            _    -> fail do "Missing type: " ++ tyConQualifiedName
    where
        tyConName = Typeable.tyConName tyCon
        tyConQualifiedName = Typeable.tyConModule tyCon ++ "." ++ tyConName
