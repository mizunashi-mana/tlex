module Language.Lexer.Tlex.Prelude.Core
    (
        module Prelude,
        module Control.Applicative,
        module Control.Monad,
        module Control.Monad.IO.Class,
        module Control.Monad.Trans.State.Strict,
        module Data.ByteString,
        module Data.Coerce,
        module Data.Foldable,
        module Data.Function,
        module Data.Functor,
        module Data.Functor.Compose,
        module Data.Ix,
        module Data.Kind,
        module Data.Ord,
        module Data.Text,
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict hiding (modify)
import           Data.ByteString                  (ByteString)
import           Data.Coerce
import           Data.Foldable                    hiding (foldl, foldr')
import           Data.Function                    hiding (($))
import           Data.Functor
import           Data.Functor.Compose
import           Data.Ix                          (Ix)
import           Data.Kind                        (Type)
import           Data.Ord                         (Down (..))
import           Data.Text                        (Text)
import           Prelude                          hiding (String, foldl, foldr,
                                                   head, tail, ($))
