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
import qualified Language.Haskell.TH.Syntax        as TH
import qualified Language.Lexer.Tlex.Data.EnumMap  as EnumMap
import qualified Language.Lexer.Tlex.Machine.DFA   as DFA
import qualified Language.Lexer.Tlex.Machine.State as MState
import qualified Language.Lexer.Tlex.Syntax        as Tlex
import qualified Language.Lexer.Tlex.Data.Addr     as Addr
import qualified GHC.Prim                          as Prim
import qualified Data.Bits                         as Bits
import qualified Language.Lexer.Tlex.Data.Bits                         as Bits
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
    deriving (Eq, Show, Enum, TH.Lift)

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
    s (c - 0)

tlexAccept :: Int -> Maybe TlexSemanticAction
tlexAccept = \x -> if x >= 120
        then Nothing
        else tlexArrayIndex table x
    where
        table :: TlexArray (Maybe TlexSemanticAction)
        table = tlexArray 120 [Nothing,...]
-}
data OutputContext = OutputContext
    { outputCtxStartStateTy     :: TH.Type
    , outputCtxCodeUnitTy       :: TH.Type
    , outputCtxCodeUnitBounds   :: (Int, Int)
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
        , outputTlexTransFn dfa
            do outputCtxCodeUnitBounds ctx
            tlexTransFnName

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
                    [ (fromEnum ss, TH.lift do fromEnum sn)
                    | (ss, sn) <- EnumMap.toAscList dfaInitials
                    ]
            in do
                es' <- sequence es
                pure (es', l)

outputTlexTransFn :: DFA.DFA a -> (Int, Int) -> TH.Name -> TH.Q TH.Dec
outputTlexTransFn DFA.DFA{ dfaTrans } (minUnitB, maxUnitB) fnName =
    let ubs = Bits.maxBitSize do maxUnitB - minUnitB
        um =
            do 1 `Bits.shiftL` ubs
            - 1
        l = concatMap
            do \dstState ->
                let smDef = case DFA.dstOtherTrans dstState of
                        Nothing -> -1
                        Just sm -> fromEnum sm
                    dstTrans = DFA.dstTrans dstState
                in map
                    do \i -> case IntMap.lookup i dstTrans of
                        Just sm -> fromEnum sm
                        Nothing -> smDef
                    [0..um]
            do toList dfaTrans
        sbs = Bits.maxBitSize do length dfaTrans - 1
        sbsEnum = if
            | ubs + sbs > 29 -> error "exceed over bit size limited"
            | otherwise      -> stateSize sbs
    in TH.ValD
        do TH.VarP fnName
        <$> do TH.NormalB <$>
                [e|\s c -> tlexLookupTlexTransTable
                    $(unitBitSizeExp ubs)
                    $(TH.lift sbsEnum)
                    $(tableAddrExp sbsEnum l)
                    s (c - $(TH.lift minUnitB))
                |]
        <*> pure []
    where
        unitBitSizeExp ubs = pure
            do TH.LitE do TH.IntegerL do fromIntegral ubs

        stateSize sbs
            | sbs <= 8  = TlexTransStateSize8
            | sbs <= 16 = TlexTransStateSize16
            | otherwise = TlexTransStateSize32

        tableAddrExp ss l =
            let us = case ss of
                    TlexTransStateSize8  -> 1
                    TlexTransStateSize16 -> 2
                    TlexTransStateSize32 -> 4
            in pure
                do TH.LitE
                    do TH.StringPrimL
                        do concatMap
                            do \sn -> Addr.addrCodeUnitsLE us
                                do fromEnum sn
                            do l

outputTlexAcceptFn
    :: DFA.DFA (TH.Q TH.Exp) -> (TH.Q TH.Type) -> TH.Name -> TH.Q TH.Dec
outputTlexAcceptFn DFA.DFA{ dfaTrans } semanticActionTy fnName = do
    tableValName <- TH.newName "table"
    (es, l) <- tableList
    TH.ValD
        do TH.VarP fnName
        <$> do TH.NormalB <$>
                [e|
                    \x -> if x >= $(TH.lift l)
                        then Nothing
                        else tlexArrayIndex $(pure do TH.VarE tableValName) x
                |]
        <*> sequence
                [ TH.SigD tableValName <$>
                    [t|TlexArray (Maybe $(semanticActionTy))|]
                , tableDec tableValName es l
                ]
    where
        tableDec valName es l = TH.ValD
            do TH.VarP valName
            <$> do TH.NormalB <$> outputTlexArrayLit l es
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

outputTlexArrayLit :: Int -> [TH.Exp] -> TH.Q TH.Exp
outputTlexArrayLit l es =
    [e|tlexArray $(TH.lift l) $(pure do TH.ListE es)|]

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
