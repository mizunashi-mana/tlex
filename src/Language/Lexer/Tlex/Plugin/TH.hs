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

import qualified Debug.Trace                              as Debug


data THScanner e = THScanner
    { thScannerOutputCtx   :: TlexTH.OutputContext
    , thScannerTlexScanner :: Tlex.Scanner e (TH.Q TH.Exp)
    }

data THScannerBuilderContext s e a = THScannerBuilderContext
    { thScannerBuilderCtxOutputCtx :: TlexTH.OutputContext
    , thScannerBuilderCtxTlexScannerBuilderCtx :: Tlex.ScannerBuilderContext s e (TH.Q TH.Exp)
    }

type THScannerBuilder s e a = State (THScannerBuilderContext s e a)

buildTHScanner :: forall e s a. Enum e => Bounded e
    => TH.Type -> TH.Type -> TH.Type -> THScannerBuilder s e a ()
    -> THScanner e
buildTHScanner codeUnitTy startStateTy actionTy builder =
    let outputCtx = TlexTH.OutputContext
            { outputCtxStartStateTy = startStateTy
            , outputCtxCodeUnitTy = codeUnitTy
            , outputCtxCodeUnitBounds = (
                fromEnum do minBound @e,
                fromEnum do maxBound @e
            )
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

buildTHScannerWithReify :: forall s a e.
    Enum e => Bounded e => Typeable e => Typeable s => Typeable a
    => THScannerBuilder s e a () -> TH.Q (THScanner e)
buildTHScannerWithReify builder = do
    startStateTy <- TypeableTH.liftTypeFromTypeable do Proxy @s
    codeUnitTy <- TypeableTH.liftTypeFromTypeable do Proxy @e
    actionTy <- TypeableTH.liftTypeFromTypeable do Proxy @a
    pure do buildTHScanner codeUnitTy startStateTy actionTy builder

liftTlexScannerBuilder :: Enum e => Tlex.ScannerBuilder s e (TH.Q TH.Exp) a -> THScannerBuilder s e f a
liftTlexScannerBuilder builder = do
    ctx0 <- get
    let (x, tlexCtx1) = runState
            do builder
            do thScannerBuilderCtxTlexScannerBuilderCtx ctx0
    put do ctx0
            { thScannerBuilderCtxTlexScannerBuilderCtx = tlexCtx1
            }
    pure x

thLexRule :: Enum e => Enum s => [s] -> Tlex.Pattern e -> TH.Q (TH.TExp a) -> THScannerBuilder s e a ()
thLexRule ss p act = liftTlexScannerBuilder do Tlex.lexRule ss p do TH.unType <$> act


outputScanner :: Enum e => THScanner e -> TH.Q [TH.Dec]
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

instance Enum e => TlexTH.TlexContext (InputStringContext e) e (InputString e) where
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
