module Language.Lexer.Tlex.Plugin.TH (
    TlexTH.TlexContext (..),
    TlexTH.TlexResult (..),
    THScanner (..),
    THScannerBuilderContext,
    THScannerBuilder,
    buildTHScanner,
    buildTHScannerWithReify,
    liftTlexScannerBuilder,
    thLexRule,
    outputScanner,
    InputString (..),
    InputStringContext (..),
    runInputString,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Language.Haskell.TH                      as TH
import qualified Language.Lexer.Tlex.Data.TypeableTH      as TypeableTH
import qualified Language.Lexer.Tlex.Machine.NFA          as NFA
import qualified Language.Lexer.Tlex.Output.TH            as TlexTH
import qualified Language.Lexer.Tlex.Pipeline.MinDfa      as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.Nfa2Dfa     as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.Pattern2Nfa as TlexPipeline
import qualified Language.Lexer.Tlex.Syntax               as Tlex

import qualified Debug.Trace as Debug


data THScanner = THScanner
    { thScannerOutputCtx   :: TlexTH.OutputContext
    , thScannerTlexScanner :: Tlex.Scanner (TH.Q TH.Exp)
    }


data THScannerBuilderContext s a = THScannerBuilderContext
    { thScannerBuilderCtxOutputCtx :: TlexTH.OutputContext
    , thScannerBuilderCtxTlexScannerBuilderCtx :: Tlex.ScannerBuilderContext s (TH.Q TH.Exp)
    }

type THScannerBuilder s a = State (THScannerBuilderContext s a)

buildTHScanner :: TH.Type -> TH.Type -> THScannerBuilder s a () -> THScanner
buildTHScanner startStateTy actionTy builder =
    let outputCtx = TlexTH.OutputContext
            { outputCtxStartStateTy = startStateTy
            , outputCtxSemanticActionTy = actionTy
            }
        tlexScanner = Tlex.buildScanner do
            modify' \ctx0 -> thScannerBuilderCtxTlexScannerBuilderCtx
                do execState builder do
                    THScannerBuilderContext
                        { thScannerBuilderCtxOutputCtx = outputCtx
                        , thScannerBuilderCtxTlexScannerBuilderCtx = ctx0
                        }
    in THScanner
        { thScannerOutputCtx = outputCtx
        , thScannerTlexScanner = tlexScanner
        }

buildTHScannerWithReify :: forall s a. Typeable s => Typeable a => THScannerBuilder s a () -> TH.Q THScanner
buildTHScannerWithReify builder = do
    startStateTy <- TypeableTH.liftTypeFromTypeable do Proxy @s
    actionTy <- TypeableTH.liftTypeFromTypeable do Proxy @a
    let outputCtx = TlexTH.OutputContext
            { outputCtxStartStateTy = startStateTy
            , outputCtxSemanticActionTy = actionTy
            }
        tlexScanner = Tlex.buildScanner do
            modify' \ctx0 -> thScannerBuilderCtxTlexScannerBuilderCtx
                do execState builder do
                    THScannerBuilderContext
                        { thScannerBuilderCtxOutputCtx = outputCtx
                        , thScannerBuilderCtxTlexScannerBuilderCtx = ctx0
                        }
    pure do
        THScanner
            { thScannerOutputCtx = outputCtx
            , thScannerTlexScanner = tlexScanner
            }

liftTlexScannerBuilder :: Tlex.ScannerBuilder s (TH.Q TH.Exp) a -> THScannerBuilder s f a
liftTlexScannerBuilder builder = do
    ctx0 <- get
    let (x, tlexCtx1) = runState
            do builder
            do thScannerBuilderCtxTlexScannerBuilderCtx ctx0
    put do ctx0
            { thScannerBuilderCtxTlexScannerBuilderCtx = tlexCtx1
            }
    pure x

thLexRule :: Enum s => [s] -> Tlex.Pattern -> TH.Q (TH.TExp a) -> THScannerBuilder s a ()
thLexRule ss p act = liftTlexScannerBuilder do Tlex.lexRule ss p do TH.unType <$> act


outputScanner :: THScanner -> TH.Q [TH.Dec]
outputScanner scanner =
    let outputCtx = thScannerOutputCtx scanner
        nfa = Debug.trace "building NFA..." do
            NFA.buildNFA do
                TlexPipeline.scanner2Nfa do thScannerTlexScanner scanner
        dfa = Debug.trace "building DFA..." do
            TlexPipeline.nfa2Dfa nfa
        minDfa = Debug.trace "minizing DFA..." do
            TlexPipeline.minDfa dfa
    in Debug.trace "outputing DFA..." do
        TlexTH.outputDfa outputCtx minDfa


data InputStringContext = InputStringContext
    { inputStringCtxRest :: [Char]
    , inputStringCtxPos  :: Int
    }
    deriving (Eq, Show)

initialInputStringContext :: [Char] -> InputStringContext
initialInputStringContext s = InputStringContext
    { inputStringCtxRest = s
    , inputStringCtxPos = 0
    }

newtype InputString a = InputString
    { unInputString :: State InputStringContext a
    }
    deriving Functor
    deriving (Applicative, Monad) via State InputStringContext

runInputString :: InputString a -> [Char] -> (a, InputStringContext)
runInputString (InputString runner) input =
    runState runner do initialInputStringContext input

instance TlexTH.TlexContext InputStringContext InputString where
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
