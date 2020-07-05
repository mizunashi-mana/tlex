{-# LANGUAGE TemplateHaskell #-}

module Language.Lexer.Tlex.Data.TypeableTH (
    liftTypeFromTypeable,
    tyConToType,
) where

import           Control.Monad
import           Data.Foldable       (foldl')
import           Prelude

import qualified Data.Typeable       as Typeable
import qualified Language.Haskell.TH as TH

liftTypeFromTypeable :: Typeable.Typeable a => Typeable.Proxy a -> TH.Q (Maybe TH.Type)
liftTypeFromTypeable p = go do Typeable.typeRep p where
    go r0 =
        let (tyCon, rs) = Typeable.splitTyConApp r0
        in tyConToType tyCon >>= \case
            Nothing -> pure Nothing
            Just t0 -> do
                mts <- forM rs \r -> go r
                pure do
                    fmap
                        do \ts -> foldl'
                            do \t1 t2 -> TH.AppT t1 t2
                            do t0
                            do ts
                        do sequence mts

tyConToType :: Typeable.TyCon -> TH.Q (Maybe TH.Type)
tyConToType tyCon = do
    mt <- TH.lookupTypeName tyConQualifiedName
    pure do fmap TH.VarT mt
    where
        tyConQualifiedName = Typeable.tyConModule tyCon ++ "." ++ Typeable.tyConName tyCon
