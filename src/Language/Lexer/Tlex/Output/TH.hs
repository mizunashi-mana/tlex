{-# LANGUAGE TemplateHaskell #-}

module Language.Lexer.Tlex.Output.TH (
    TlexContext (..),
    TlexResult (..),
    outputDfa,
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Haskell.TH as TH
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Machine.DFA as DFA
import qualified Language.Lexer.Tlex.Data.EnumMap as EnumMap
import qualified Language.Lexer.Tlex.Syntax as Tlex


class Monad m => TlexContext m where
    tlexGetInputPart :: m (Maybe Char)

data TlexResult a
    = TlexEndOfInput
    | TlexError
    | TlexAccepted a

{-
type StartState = ...
type SemanticAction = ...

tlexScan :: TlexContext m => StartState -> m (TlexResult SemanticAction)
tlexScan s0 = go (tlexInitial s0) where
    go s = case tlexAccept s of
        Just x  -> pure (TlexAccepted x)
        Nothing -> do
            mc <- tlexGetInputPart
            case mc of
                Nothing -> pure TlexEndOfInput
                Just c  -> goTrans s c

    goTrans s c =  case tlexTrans s c of
        -1 -> pure TlexError
        ns -> do
            mc <- tlexGetInputPart
            case mc of
                Nothing -> pure TlexError
                Just c  -> goTrans ns c

tlexInitial :: StartState -> Int
tlexInitial x = case fromEnum x of
    1 -> 10
    ...
    _ -> error "unavailable start state"

tlexTrans :: Int -> Char -> Int
tlexTrans sf c = case sf of
    1 -> case c of
        'a' -> 2
        ...
        _ -> 4
    ...
    _ -> -1

tlexAccept :: Int -> Maybe SemanticAction
tlexAccept i = case i of
    1 -> Just ...
    ...
    _ -> Nothing
-}
outputDfa :: DFA.DFA a -> TH.Q [TH.Dec]
outputDfa dfa = do
    let startStateTyName = TH.mkName "StartState"
        semanticActionTyName = TH.mkName "SemanticAction"
        tlexScanFnName = TH.mkName "tlexScan"
        tlexInitialFnName = TH.mkName "tlexInitial"
        tlexTransFnName = TH.mkName "tlexTrans"
        tlexAcceptFnName = TH.mkName "tlexAccept"

    let startStateTy = pure @TH.Q do TH.ConT startStateTyName
        semanticActionTy = pure @TH.Q do TH.ConT semanticActionTyName
        tlexInitialFn = pure @TH.Q do TH.VarE tlexInitialFnName
        tlexTransFn = pure @TH.Q do TH.VarE tlexTransFnName
        tlexAcceptFn = pure @TH.Q do TH.VarE tlexAcceptFnName

    sequence
        [ TH.TySynD startStateTyName [] <$> [t|Int|]
        , TH.TySynD semanticActionTyName [] <$> [t|()|]

        , TH.SigD tlexScanFnName <$>
            [t|forall m. TlexContext m => $(startStateTy) -> m (TlexResult $(semanticActionTy))|]
        , TH.ValD
            do TH.VarP tlexScanFnName
            <$> do TH.NormalB <$> [e|\s0 -> go ($(tlexInitialFn) s0)|]
            <*> [d|
                go s = case $(tlexAcceptFn) s of
                    Just x  -> pure (TlexAccepted x)
                    Nothing -> do
                        mc <- tlexGetInputPart
                        case mc of
                            Nothing -> pure TlexEndOfInput
                            Just c  -> goTrans s c

                goTrans s c = case $(tlexTransFn) s c of
                    -1 -> pure TlexError
                    ns -> case $(tlexAcceptFn) ns of
                        Just x -> pure (TlexAccepted x)
                        Nothing -> do
                            mc <- tlexGetInputPart
                            case mc of
                                Nothing -> pure TlexError
                                Just nc -> goTrans ns nc
            |]
        
        , TH.SigD tlexInitialFnName <$>
            [t|$(startStateTy) -> Int|]
        , outputTlexInitialFn dfa tlexInitialFnName

        , TH.SigD tlexTransFnName <$>
            [t|Int -> Char -> Int|]
        , outputTlexTransFn dfa tlexTransFnName

        , TH.SigD tlexAcceptFnName <$>
            [t|Int -> Maybe $(semanticActionTy)|]
        , outputTlexAcceptFn dfa tlexAcceptFnName
        ]

outputTlexInitialFn :: DFA.DFA a -> TH.Name -> TH.Q TH.Dec
outputTlexInitialFn DFA.DFA{ dfaInitials } fnName = TH.ValD
    do TH.VarP fnName
    <$> do TH.NormalB <$> do
            xVarName <- TH.newName "x"
            TH.LamE [TH.VarP xVarName] <$> do
                TH.CaseE
                    <$> [e|fromEnum $(pure do TH.VarE xVarName)|]
                    <*> sequence do
                        [ pure do
                            TH.Match
                                do TH.LitP do outputStartState k
                                do TH.NormalB do TH.LitE do outputStateNum v
                                do []
                            | (k, v) <- EnumMap.assocs dfaInitials
                            ] ++
                            [ TH.Match
                                <$> [p|_|]
                                <*> do TH.NormalB <$> [e|error "unavailable start state"|]
                                <*> pure []
                            ]
    <*> pure []

outputTlexTransFn :: DFA.DFA a -> TH.Name -> TH.Q TH.Dec
outputTlexTransFn = undefined

outputTlexAcceptFn :: DFA.DFA a -> TH.Name -> TH.Q TH.Dec
outputTlexAcceptFn = undefined

outputStartState :: Tlex.StartState -> TH.Lit
outputStartState x = TH.IntegerL do fromIntegral do fromEnum x

outputStateNum :: MState.StateNum -> TH.Lit
outputStateNum x = TH.IntegerL do fromIntegral do fromEnum x
