{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Lexer.Tlex.Output.TH (
    TlexContext (..),
    TlexResult (..),
    Runner (..),
    runRunner,
    TlexTransStateSize (..),
    tlexLookupTlexTransTable,
    TlexArray,
    tlexArray,
    tlexArrayIndex,
    OutputContext (..),
    outputDfa,

    -- for tests
    addrCodeUnitsLE,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.Array                        as Array
import qualified Data.Bits                         as Bits
import qualified Data.IntMap.Strict                as IntMap
import qualified GHC.Prim                          as Prim
import qualified GHC.ST                            as ST
import qualified GHC.Types                         as Types
import qualified Language.Haskell.TH               as TH
import qualified Language.Haskell.TH.Syntax        as TH
import qualified Language.Lexer.Tlex.Data.Bits     as Bits
import qualified Data.EnumMap.Strict  as EnumMap
import qualified Language.Lexer.Tlex.Machine.DFA   as DFA
import qualified Language.Lexer.Tlex.Machine.State as MState
import           Language.Lexer.Tlex.Runner
import qualified Language.Lexer.Tlex.Syntax        as Tlex


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
        do ST.ST \s0# -> case unitSize of
            TlexTransStateSize8  -> case Prim.readWord8OffAddr# table# i# s0# of
                (# s1#, r# #) -> case r# of
                    255##   -> (# s1#, -1 #)
                    _       -> (# s1#, Types.I# do Prim.word2Int# r# #)
            TlexTransStateSize16 -> case Prim.readWord16OffAddr# table# i# s0# of
                (# s1#, r# #) -> case r# of
                    65535## -> (# s1#, -1 #)
                    _       -> (# s1#, Types.I# do Prim.word2Int# r# #)
            TlexTransStateSize32 -> case Prim.readInt32OffAddr# table# i# s0# of
                (# s1#, r# #) -> (# s1#, Types.I# r# #)

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
tlexScan s0 = runRunner runner s0
    where
        runner = Runner
            { tlexInitial = thTlexInitial
            , tlexAccept = thTlexAccept
            , tlexTrans = thTlexTrans
            }

thTlexInitial :: Int -> Int
thTlexInitial = \x -> tlexArrayIndex tlexInitialTable x
    where
        table :: TlexArray Int
        table = tlexArray 10 [10,...]

thTlexTrans :: Int -> Int -> Int
thTlexTrans = \s c -> tlexLookupTlexTransTable
    8
    TlexTransTableStateSize8
    "\x02\x00\x00\x00..."#
    s (c - 0)

thTlexAccept :: Int -> Maybe TlexSemanticAction
thTlexAccept = \x -> if x >= 120
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
        thTlexInitialFnName = TH.mkName "thTlexInitial"
        thTlexTransFnName = TH.mkName "thTlexTrans"
        thTlexAcceptFnName = TH.mkName "thTlexAccept"

    let startStateTy = pure @TH.Q do TH.ConT startStateTyName
        codeUnitTy = pure @TH.Q do TH.ConT codeUnitTyName
        semanticActionTy = pure @TH.Q do TH.ConT semanticActionTyName
        thTlexInitialFn = pure @TH.Q do TH.VarE thTlexInitialFnName
        thTlexTransFn = pure @TH.Q do TH.VarE thTlexTransFnName
        thTlexAcceptFn = pure @TH.Q do TH.VarE thTlexAcceptFnName

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
            <$> do TH.NormalB <$> [e|\s0 -> runRunner runner s0|]
            <*> [d|
                runner = Runner
                    $(thTlexInitialFn)
                    $(thTlexAcceptFn)
                    $(thTlexTransFn)
            |]

        , TH.SigD thTlexInitialFnName <$>
            [t|Int -> Int|]
        , outputTlexInitialFn dfa thTlexInitialFnName

        , TH.SigD thTlexTransFnName <$>
            [t|Int -> Int -> Int|]
        , outputTlexTransFn dfa
            do outputCtxCodeUnitBounds ctx
            thTlexTransFnName

        , TH.SigD thTlexAcceptFnName <$>
            [t|Int -> Maybe $(semanticActionTy)|]
        , outputTlexAcceptFn dfa semanticActionTy thTlexAcceptFnName
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
        um = do 1 `Bits.shiftL` ubs
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
        -- count of states + count of specials (i.e. -1)
        sbs = Bits.maxBitSize do (length dfaTrans - 1) + 1
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
                            do \sn -> addrCodeUnitsLE us
                                do fromEnum sn
                            do l

-- | Should correspond @tlexLookupTlexTransTable@
addrCodeUnitsLE :: Bits.Bits a => Integral a => Int -> a -> [Word8]
addrCodeUnitsLE us n
    | n >= 0    = take us
        do map
            do \m -> fromInteger do toInteger do mod8bit m
            do iterate (`Bits.shiftR` 8) n
    | n == -1   = replicate us 0xFF
    | otherwise = error "unsupported"
    where
        mod8bit = case Bits.bitSizeMaybe n of
            Nothing -> \x -> x Bits..&. 0xFF
            Just bs
                | bs <= 8   -> \x -> x
                | otherwise -> \x -> x Bits..&. 0xFF

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
