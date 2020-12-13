module Main where

import qualified Lexer
import qualified System.Environment as System
import qualified System.Exit        as System


main :: IO ()
main = do
    args <- System.getArgs
    f <- case args of
        [] -> do
            putStrLn "need input path"
            System.exitFailure
        x:_ -> pure x
    s <- ByteString.readFile f
    case Lexer.lexByteString s of
        Right xs -> print xs
        Left msg -> do
            putStrLn "error: "
            putStrLn msg
            System.exitFailure
