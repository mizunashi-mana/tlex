module Lexer.TextId (
  T,
  TextId (..),
  stringLit,
) where


type T = TextId

newtype TextId = TextId String
    deriving (Eq, Ord, Show)

stringLit :: String -> TextId
stringLit str = TextId str
