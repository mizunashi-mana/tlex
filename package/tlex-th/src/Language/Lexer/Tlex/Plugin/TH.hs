{-# LANGUAGE CPP #-}

module Language.Lexer.Tlex.Plugin.TH (
    TlexTH.TlexContext (..),
    TlexTH.TlexResult (..),
    TlexTH.Runner (..),
    TlexTH.runRunner,
    THScanner (..),
    THScannerBuilderContext,
    THScannerBuilder,
    buildTHScanner,
    buildTHScannerWithReify,
    liftTlexScannerBuilder,
    thLexRule,
    outputScanner,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Language.Haskell.TH                      as TH
import qualified Language.Lexer.Tlex.Data.TypeableTH      as TypeableTH
import qualified Language.Lexer.Tlex.Machine.NFA          as NFA
import qualified Language.Lexer.Tlex.Output.TH            as TlexTH
import qualified Language.Lexer.Tlex.Pipeline.MinDfa      as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.Nfa2Dfa     as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.Scanner2Nfa as TlexPipeline
import qualified Language.Lexer.Tlex.Syntax               as Tlex

#ifdef DEBUG
import qualified Debug.Trace                              as Debug
#endif


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
        nfa =
#ifdef DEBUG
            Debug.trace "building NFA..." do
#endif
            NFA.buildNFA do
                TlexPipeline.scanner2Nfa do thScannerTlexScanner scanner
        dfa =
#ifdef DEBUG
            Debug.trace "building DFA..." do
#endif
            TlexPipeline.nfa2Dfa nfa
        minDfa =
#ifdef DEBUG
            Debug.trace "minizing DFA..." do
#endif
            TlexPipeline.minDfa dfa
    in
#ifdef DEBUG
        Debug.trace "outputing DFA..." do
#endif
        TlexTH.outputDfa outputCtx minDfa
