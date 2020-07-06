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
    runInputString,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Language.Haskell.TH                      as TH
import qualified Language.Lexer.Tlex.Data.TypeableTH      as TypeableTH
import qualified Language.Lexer.Tlex.Machine.NFA          as NFA
import qualified Language.Lexer.Tlex.Output.TH            as TlexTH
import qualified Language.Lexer.Tlex.Pipeline.Nfa2Dfa     as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.Pattern2Nfa as TlexPipeline
import qualified Language.Lexer.Tlex.Syntax               as Tlex


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

buildTHScannerWithReify :: forall s a. Typeable s => Typeable a => THScannerBuilder s a () -> THScanner
buildTHScannerWithReify builder =
    let startStateTy = TypeableTH.liftTypeFromTypeable do Proxy @s
        actionTy = TypeableTH.liftTypeFromTypeable do Proxy @s
        outputCtx = TlexTH.OutputContext
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
        nfa = NFA.buildNFA do
            TlexPipeline.scanner2Nfa do thScannerTlexScanner scanner
        dfa = TlexPipeline.nfa2Dfa nfa
    in TlexTH.outputDfa outputCtx dfa


newtype InputString a = InputString (State [Char] a)
    deriving Functor
    deriving (Applicative, Monad) via State [Char]


instance TlexTH.TlexContext [Char] InputString where
    tlexGetInputPart = InputString do
        input <- get
        case input of
            []  -> pure Nothing
            c:r -> do
                put r
                pure do Just c
    tlexGetMark = InputString get

runInputString :: InputString a -> [Char] -> ([Char], a)
runInputString (InputString builder) input =
    let (x, s) = runState builder input
    in (s, x)
