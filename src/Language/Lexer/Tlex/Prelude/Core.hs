module Language.Lexer.Tlex.Prelude.Core
    (
        module Prelude,
        module Control.Applicative,
        module Control.Monad,
        module Control.Monad.IO.Class,
        module Data.ByteString,
        module Data.Coerce,
        module Data.Foldable,
        module Data.Functor,
        module Data.Ix,
        module Data.Kind,
        module Data.MonoTraversable,
        module Data.Text,
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString                   (ByteString)
import           Data.Coerce
import           Data.Foldable                     hiding (foldl, foldr')
import           Data.Functor
import           Data.Ix                           (Ix)
import           Data.Kind                         (Type)
import           Data.MonoTraversable              hiding (omapM, oforM)
import           Data.Text                         (Text)
import           Prelude                           hiding (foldl, foldr, head, tail, String)
