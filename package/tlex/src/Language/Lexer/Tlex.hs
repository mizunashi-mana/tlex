module Language.Lexer.Tlex (
    module Language.Lexer.Tlex.Syntax,
    module Language.Lexer.Tlex.Runner,
    module Language.Lexer.Tlex.Data.InputString,
    buildRunner,
) where

import           Language.Lexer.Tlex.Data.InputString
import           Language.Lexer.Tlex.Prelude
import           Language.Lexer.Tlex.Runner
import           Language.Lexer.Tlex.Syntax

import qualified Language.Lexer.Tlex.Machine.NFA          as NFA
import qualified Language.Lexer.Tlex.Pipeline.Dfa2Runner  as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.MinDfa      as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.Nfa2Dfa     as TlexPipeline
import qualified Language.Lexer.Tlex.Pipeline.Scanner2Nfa as TlexPipeline


buildRunner :: Enum e => Scanner e a -> Runner e a
buildRunner scanner =
    let nfa = NFA.buildNFA do TlexPipeline.scanner2Nfa scanner
        dfa = TlexPipeline.nfa2Dfa nfa
        minDfa = TlexPipeline.minDfa dfa
    in TlexPipeline.dfa2Runner minDfa
