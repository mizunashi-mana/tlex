{-# LANGUAGE TemplateHaskell #-}

module Lexer.Debug where

import qualified Language.Haskell.TH                      as TH
import qualified Language.Lexer.Tlex.Machine.NFA          as NFA
import qualified Language.Lexer.Tlex.Pipeline.MinDfa      as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.Nfa2Dfa     as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.Pattern2Nfa as TlexPipeline
import qualified Language.Lexer.Tlex.Plugin.TH            as TlexTH
import qualified Lexer.Rules                              as LexerRules


lexer = TlexTH.buildTHScanner
    (TH.ConT (TH.mkName "LexerState"))
    (TH.ConT (TH.mkName "LexerCodeUnit"))
    (TH.TupleT 0)
    LexerRules.lexerRules
tlexLexer = TlexTH.thScannerTlexScanner lexer
nfa = NFA.buildNFA $ TlexPipeline.scanner2Nfa tlexLexer
dfa = TlexPipeline.nfa2Dfa nfa
minDfa = TlexPipeline.minDfa dfa
lexerProgram = do
    ast <- TH.runQ LexerRules.buildLexer
    print $ TH.ppr ast
