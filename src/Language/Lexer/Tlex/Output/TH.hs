{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash       #-}

module Language.Lexer.Tlex.Output.TH (
    TlexContext (..),
    TlexResult (..),
    TlexTransStateSize (..),
    tlexLookupTlexTransTable,
    TlexArray,
    tlexArray,
    tlexArrayIndex,
    OutputContext (..),
    outputDfa,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.IntMap.Strict                as IntMap
import qualified Language.Haskell.TH               as TH
import qualified Language.Lexer.Tlex.Data.EnumMap  as EnumMap
import qualified Language.Lexer.Tlex.Machine.DFA   as DFA
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Syntax        as Tlex
import qualified GHC.Prim                          as Prim
import qualified Data.Bits                         as Bits
import qualified GHC.ST                            as ST
import qualified Data.Array                        as Array
import qualified GHC.Types                         as Types


class (Enum e, Monad m) => TlexContext s e m | m -> s, m -> e where
    tlexGetInputPart :: m (Maybe e)
    tlexGetMark :: m s

data TlexResult s a
    = TlexEndOfInput
    | TlexError
    | TlexAccepted s a
    deriving (Eq, Show)

data TlexTransStateSize
    = TlexTransStateSize8
    | TlexTransStateSize16
    | TlexTransStateSize32
    deriving (Eq, Show, Enum)

{-# INLINE tlexLookupTlexTransTable #-}
tlexLookupTlexTransTable :: Int -> TlexTransStateSize -> Prim.Addr#
    -> Int -> Int -> Int
tlexLookupTlexTransTable offset unitSize table# s c =
    let !(Types.I# i#) = s `Bits.shiftL` offset + c
    in ST.runST
        do ST.ST \s0# ->
            let !(# s1#, r# #) = case unitSize of
                    TlexTransStateSize8  -> Prim.readInt8OffAddr#  table# i# s0#
                    TlexTransStateSize16 -> Prim.readInt16OffAddr# table# i# s0#
                    TlexTransStateSize32 -> Prim.readInt32OffAddr# table# i# s0#
            in (# s1#, Types.I# r# #)

type TlexArray = Array.Array Int

{-# INLINE tlexArray #-}
tlexArray :: Int -> [a] -> TlexArray a
tlexArray l xs = Array.listArray (0,l) xs

{-# INLINE tlexArrayIndex #-}
tlexArrayIndex :: TlexArray a -> Int -> a
tlexArrayIndex arr i = arr Array.! i

{-
type TlexStartState = ...
type TlexSemanticAction = ...
type TlexCodeUnit = ...

tlexScan :: TlexContext s TlexCodeUnit m => TlexStartState -> m (TlexResult s TlexSemanticAction)
tlexScan s0 = go (tlexInitial (fromEnum s0)) where
    go s = case tlexAccept s of
        Just x  -> do
            acc <- buildAccepted x
            mc <- tlexGetInputPart
            case mc of
                Nothing -> pure acc
                Just c  -> goTrans s c (Just acc)
        Nothing -> do
            mc <- tlexGetInputPart
            case mc of
                Nothing -> pure TlexEndOfInput
                Just c  -> goTrans s c Nothing

    goTrans s c preAccepted = case tlexTrans s (fromEnum c) of
        -1 -> goEnd preAccepted
        ns -> do
            nacc <- case tlexAccept ns of
                Just x -> do
                    acc <- buildAccepted x
                    pure (Just acc)
                Nothing -> pure preAccepted
            mc <- tlexGetInputPart
            case mc of
                Nothing -> goEnd nacc
                Just nc -> goTrans ns nc nacc

    buildAccepted x = do
        m <- tlexGetMark
        pure (TlexAccepted m x)

    goEnd preAccepted = case preAccepted of
        Nothing  -> pure TlexError
        Just acc -> pure acc

tlexInitial :: Int -> Int
tlexInitial = \x -> tlexArrayIndex table x
    where
        table :: TlexArray Int
        table = tlexArray 10 [10,...]

tlexTrans :: Int -> Int -> Int
tlexTrans = \s c -> tlexLookupTlexTransTable
    8
    TlexTransTableStateSize8
    "\x02\x00\x00\x00..."#
    s c

tlexAccept :: Int -> Maybe TlexSemanticAction
tlexAccept = \x -> tlexArrayIndex table x
    where
        table :: TlexArray (Maybe TlexSemanticAction)
        table = tlexArray 120 [Nothing,...]
-}
data OutputContext = OutputContext
    { outputCtxStartStateTy     :: TH.Type
    , outputCtxCodeUnitTy       :: TH.Type
    , outputCtxSemanticActionTy :: TH.Type
    }
    deriving (Eq, Show)

outputDfa :: OutputContext -> DFA.DFA (TH.Q TH.Exp) -> TH.Q [TH.Dec]
outputDfa ctx dfa = do
    let startStateTyName = TH.mkName "TlexStartState"
        codeUnitTyName = TH.mkName "TlexCodeUnit"
        semanticActionTyName = TH.mkName "TlexSemanticAction"
        tlexScanFnName = TH.mkName "tlexScan"
        tlexInitialFnName = TH.mkName "tlexInitial"
        tlexTransFnName = TH.mkName "tlexTrans"
        tlexAcceptFnName = TH.mkName "tlexAccept"

    let startStateTy = pure @TH.Q do TH.ConT startStateTyName
        codeUnitTy = pure @TH.Q do TH.ConT codeUnitTyName
        semanticActionTy = pure @TH.Q do TH.ConT semanticActionTyName
        tlexInitialFn = pure @TH.Q do TH.VarE tlexInitialFnName
        tlexTransFn = pure @TH.Q do TH.VarE tlexTransFnName
        tlexAcceptFn = pure @TH.Q do TH.VarE tlexAcceptFnName

    sequence
        [ pure do TH.TySynD startStateTyName [] do outputCtxStartStateTy ctx
        , pure do TH.TySynD codeUnitTyName [] do outputCtxCodeUnitTy ctx
        , pure do TH.TySynD semanticActionTyName [] do outputCtxSemanticActionTy ctx

        , TH.SigD tlexScanFnName <$> [t|
            forall s m. TlexContext s $(codeUnitTy) m
                => $(startStateTy) -> m (TlexResult s $(semanticActionTy))
        |]
        , TH.ValD
            do TH.VarP tlexScanFnName
            <$> do TH.NormalB <$> [e|\s0 -> go ($(tlexInitialFn) (fromEnum s0))|]
            <*> [d|
                go s = case $(tlexAcceptFn) s of
                    Just x  -> do
                        acc <- buildAccepted x
                        mc <- tlexGetInputPart
                        case mc of
                            Nothing -> pure acc
                            Just c  -> goTrans s c (Just acc)
                    Nothing -> do
                        mc <- tlexGetInputPart
                        case mc of
                            Nothing -> pure TlexEndOfInput
                            Just c  -> goTrans s c Nothing

                goTrans s c preAccepted = case $(tlexTransFn) s (fromEnum c) of
                    -1 -> goEnd preAccepted
                    ns -> do
                        nacc <- case $(tlexAcceptFn) ns of
                            Just x -> do
                                acc <- buildAccepted x
                                pure (Just acc)
                            Nothing -> pure preAccepted
                        mc <- tlexGetInputPart
                        case mc of
                            Nothing -> goEnd nacc
                            Just nc -> goTrans ns nc nacc

                buildAccepted x = do
                    m <- tlexGetMark
                    pure (TlexAccepted m x)

                goEnd preAccepted = case preAccepted of
                    Nothing  -> pure TlexError
                    Just acc -> pure acc
            |]

        , TH.SigD tlexInitialFnName <$>
            [t|Int -> Int|]
        , outputTlexInitialFn dfa tlexInitialFnName

        , TH.SigD tlexTransFnName <$>
            [t|Int -> Int -> Int|]
        , outputTlexTransFn dfa tlexTransFnName

        , TH.SigD tlexAcceptFnName <$>
            [t|Int -> Maybe $(semanticActionTy)|]
        , outputTlexAcceptFn dfa semanticActionTy tlexAcceptFnName
        ]

outputTlexInitialFn :: DFA.DFA a -> TH.Name -> TH.Q TH.Dec
outputTlexInitialFn DFA.DFA{ dfaInitials } fnName = do
    tableValName <- TH.newName "table"
    TH.ValD
        do TH.VarP fnName
        <$> do TH.NormalB <$>
                [e|\x -> tlexArrayIndex $(pure do TH.VarE tableValName) x|]
        <*> sequence
                [ TH.SigD tableValName <$>
                    [t|TlexArray Int|]
                , tableDec tableValName
                ]
    where
        tableDec :: TH.Name -> TH.Q TH.Dec
        tableDec valName = TH.ValD
            do TH.VarP valName
            <$> do TH.NormalB <$> do
                    (es, l) <- tableList
                    outputTlexArrayLit l es
            <*> pure []

        tableList :: TH.Q ([TH.Exp], Int)
        tableList =
            let (es, l) = sequentialListFromAscList
                    [e|-1|]
                    [ (fromEnum ss, pure do TH.LitE do outputStateNum sn)
                    | (ss, sn) <- EnumMap.toAscList dfaInitials
                    ]
            in do
                es' <- sequence es
                pure (es', l)

outputTlexTransFn :: DFA.DFA a -> TH.Name -> TH.Q TH.Dec
outputTlexTransFn DFA.DFA{ dfaTrans } fnName = TH.ValD
    do TH.VarP fnName
    <$> do TH.NormalB <$>
            [e|\s c -> tlexLookupTlexTransTable
                $(tlexTransTableUnitBitSize)
                $(tlexTransTableStateSize)
                $(tlexTransTable)
                s c
            |]
    <*> pure []
    where
        tlexTransTableUnitBitSize = undefined
        tlexTransTableStateSize = undefined
        tlexTransTable = undefined

outputTlexAcceptFn
    :: DFA.DFA (TH.Q TH.Exp) -> (TH.Q TH.Type) -> TH.Name -> TH.Q TH.Dec
outputTlexAcceptFn DFA.DFA{ dfaTrans } semanticActionTy fnName = do
    tableValName <- TH.newName "table"
    TH.ValD
        do TH.VarP fnName
        <$> do TH.NormalB <$>
                [e|\x -> tlexArrayIndex $(pure do TH.VarE tableValName) x|]
        <*> sequence
                [ TH.SigD tableValName <$>
                    [t|TlexArray (Maybe $(semanticActionTy))|]
                , tableDec tableValName
                ]
    where
        tableDec :: TH.Name -> TH.Q TH.Dec
        tableDec valName = TH.ValD
            do TH.VarP valName
            <$> do TH.NormalB <$> do
                    (es, l) <- tableList
                    outputTlexArrayLit l es
            <*> pure []

        tableList :: TH.Q ([TH.Exp], Int)
        tableList =
            let (es, l) = sequentialListFromAscList
                    [e|Nothing|]
                    do
                        (sn, dstSt) <- MState.arrayAssocs dfaTrans
                        let accExp = case DFA.dstAccepts dstSt of
                                []    -> [e|Nothing|]
                                acc:_ -> [e|Just $(Tlex.accSemanticAction acc)|]
                        pure (fromEnum sn, accExp)
            in do
                es' <- sequence es
                pure (es', l)

outputStateNum :: MState.StateNum -> TH.Lit
outputStateNum x = TH.IntegerL do fromIntegral do fromEnum x

outputTlexArrayLit :: Int -> [TH.Exp] -> TH.Q TH.Exp
outputTlexArrayLit l es = [e|tlexArray $(arraySizeExp) $(arrayListExp)|] where
    arraySizeExp = pure do TH.LitE do TH.IntegerL do fromIntegral l
    arrayListExp = pure do TH.ListE es

sequentialListFromAscList :: a -> [(Int, a)] -> ([a], Int)
sequentialListFromAscList v xs =
    let (l0, m) = foldl'
            do \(l, !pi) (i, x) -> (fillV i pi l . (x:), succ i)
            do (id, 0)
            do xs
    in (l0 [], m)
    where
        fillV i !pi l
            | pi == i   = l
            | otherwise = fillV i
                do succ pi
                do l . (v:)
