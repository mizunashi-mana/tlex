module Language.Lexer.Tlex.Data.Reporter (
    Reporter,
    report,
    getResult,
    getReports,
    toEither,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Data.Bag as Bag


data Reporter e a = Reporter
    { getReportBag :: Bag.Bag e
    , getResult    :: a
    }
    deriving (Eq, Show, Functor)

getReports :: Reporter e a -> [e]
getReports x = toList do getReportBag x

report :: e -> Reporter e ()
report x = Reporter
    { getReportBag = Bag.singleton x
    , getResult = ()
    }

toEither :: Reporter e a -> Either [e] a
toEither x = case getReports x of
    [] -> Right do getResult x
    es -> Left es

instance Applicative (Reporter e) where
    pure x = Reporter
        { getReportBag = mempty
        , getResult = x
        }

    mf <*> mx = Reporter
        { getReportBag = getReportBag mf <> getReportBag mx
        , getResult = getResult mf do getResult mx
        }

    mx *> my = Reporter
        { getReportBag = getReportBag mx <> getReportBag my
        , getResult = getResult my
        }

instance Monad (Reporter e) where
    mx >>= f =
        let my = f do getResult mx
        in Reporter
            { getReportBag = getReportBag mx <> getReportBag my
            , getResult = getResult my
            }
