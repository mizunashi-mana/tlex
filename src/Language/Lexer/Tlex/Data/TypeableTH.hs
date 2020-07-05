{-# LANGUAGE TemplateHaskell #-}

module Language.Lexer.Tlex.Data.TypeableTH (
    liftTypeFromTypeable,
    tyConToType,
) where

import           Data.Foldable       (foldl')

import qualified Data.Typeable       as Typeable
import qualified Language.Haskell.TH as TH

liftTypeFromTypeable :: Typeable.Typeable a => Typeable.Proxy a -> TH.Type
liftTypeFromTypeable p = go do Typeable.typeRep p where
    go r0 =
        let (tyCon, rs) = Typeable.splitTyConApp r0
        in foldl'
            do \t1 t2 -> TH.AppT t1 t2
            do tyConToType tyCon
            do [ go r | r <- rs ]

tyConToType :: Typeable.TyCon -> TH.Type
tyConToType tyCon = TH.ConT do TH.mkName do Typeable.tyConName tyCon
