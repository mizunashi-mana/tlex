module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

data LexerState
    = Initial
    | NestedComment
    deriving (Eq, Show, Enum)

lexer :: LexerDeclaration
lexer = LexerDeclaration
    { initialState = Initial
    , rules = rules
    }
    where
        rules = do
            initialRule
