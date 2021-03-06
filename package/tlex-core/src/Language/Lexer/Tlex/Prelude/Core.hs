module Language.Lexer.Tlex.Prelude.Core (
    module Prelude,
    module Control.Applicative,
    module Control.Monad,
    module Control.Monad.IO.Class,
    module Control.Monad.Trans.State.Strict,
    module Data.Coerce,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Data.Functor.Identity,
    module Data.Functor.Compose,
    module Data.Ix,
    module Data.Kind,
    module Data.List.NonEmpty,
    module Data.Ord,
    module Data.Proxy,
    module Data.Typeable,
    module Data.Word,
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict hiding (modify)
import           Data.Coerce
import           Data.Foldable                    hiding (foldl, foldr')
import           Data.Function                    hiding (($))
import           Data.Functor
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Ix                          (Ix)
import           Data.Kind                        (Type)
import           Data.List.NonEmpty               (NonEmpty (..))
import           Data.Ord                         (Down (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Typeable                    (Typeable)
import           Data.Word                        (Word, Word8)
import           Prelude                          hiding (String, foldl, foldr,
                                                   head, pi, tail, ($))
