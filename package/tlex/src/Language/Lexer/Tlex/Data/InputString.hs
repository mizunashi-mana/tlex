module Language.Lexer.Tlex.Data.InputString (
    InputStringContext (..),
    InputString (..),
    runInputString,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Runner  as Tlex


data InputStringContext e = InputStringContext
    { inputStringCtxRest :: [e]
    , inputStringCtxPos  :: Int
    }
    deriving (Eq, Show)

initialInputStringContext :: [e] -> InputStringContext e
initialInputStringContext s = InputStringContext
    { inputStringCtxRest = s
    , inputStringCtxPos = 0
    }

newtype InputString e a = InputString
    { unInputString :: State (InputStringContext e) a
    }
    deriving (
        Functor,
        Applicative,
        Monad
    ) via State (InputStringContext e)

runInputString :: InputString e a -> [e] -> (a, InputStringContext e)
runInputString (InputString runner) input =
    runState runner do initialInputStringContext input

instance Enum e => Tlex.TlexContext (InputStringContext e) e (InputString e) where
    tlexGetInputPart = InputString do
        inputCtx <- get
        case inputStringCtxRest inputCtx of
            []  -> pure Nothing
            c:r -> do
                put do InputStringContext
                        { inputStringCtxRest = r
                        , inputStringCtxPos = succ do inputStringCtxPos inputCtx
                        }
                pure do Just c
    tlexGetMark = InputString get
