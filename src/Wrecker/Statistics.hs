{-# LANGUAGE RecordWildCards, BangPatterns, LambdaCase,
  OverloadedStrings, DataKinds, NamedFieldPuns #-}

module Wrecker.Statistics
    ( Statistics(..)
    , AllStats(..)
    , ResultStatistics(..)
    , stepAllStats
    , emptyAllStats
    , printStats
    , pprStats
    ) where

import Data.Aeson (ToJSON(..), Value(..), (.=), object)
import Data.Function
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.TDigest as TD
import qualified Data.Text as T
import qualified Network.URI as URI
import Text.Printf
import Text.Tabular
import qualified Text.Tabular.AsciiArt as AsciiArt
import Wrecker.Options
import Wrecker.Recorder

insertHist :: Double -> TD.TDigest 5 -> TD.TDigest 5
insertHist = TD.insert

-- | These are the
data Statistics = Statistics
    { sHistogram :: TD.TDigest 5
      -- ^ A histogram of times
    , sTotal :: {-# UNPACK #-}!Double
      -- ^ The total time
    } deriving (Show)

-- | Extract the mean
mean :: Statistics -> Double
mean = fromMaybe 0 . TD.mean . sHistogram

-- | Extract the variance
variance :: Statistics -> Double
variance = fromMaybe 0 . TD.variance . sHistogram

quantile95 :: Statistics -> Double
quantile95 = fromMaybe 0 . TD.quantile 0.95 . sHistogram

statsCount :: Statistics -> Int
statsCount = floor . (+ 0.1) . TD.totalWeight . sHistogram

minimumValue :: Statistics -> Double
minimumValue = TD.minimumValue . sHistogram

maximumValue :: Statistics -> Double
maximumValue = TD.maximumValue . sHistogram

emptyStatistics :: Statistics
emptyStatistics = Statistics {sHistogram = mempty, sTotal = 0}

stepStatistics :: Statistics -> Double -> Statistics
stepStatistics !stats !value =
    stats
    { sHistogram = insertHist value (sHistogram stats) -- insert new value in histogram
    , sTotal = sTotal stats + value
    }

urlToPathPieceKey :: String -> String
urlToPathPieceKey url = maybe url URI.uriPath $ URI.parseURI url

{- | This type includes statistics for all of the result values we can detect.
     This type is used by AllStats to compute per key (URL) statistics among
     other uses.
-}
data ResultStatistics = ResultStatistics
    { rs2xx :: !Statistics
    , rs4xx :: !Statistics
    , rs5xx :: !Statistics
    , rsFailed :: !Statistics
    , rsRollup :: !Statistics
    , rsTotalTests :: !Int
    , rsTestsFailed :: !Int
    } deriving (Show)

emptyResultStatistics :: ResultStatistics
emptyResultStatistics =
    ResultStatistics
    { rs2xx = emptyStatistics
    , rs4xx = emptyStatistics
    , rs5xx = emptyStatistics
    , rsFailed = emptyStatistics
    , rsRollup = emptyStatistics
    , rsTotalTests = 0
    , rsTestsFailed = 0
    }

stepResultStatistics :: ResultStatistics -> RunResult -> ResultStatistics
stepResultStatistics !stats =
    \case
        Success {resultTime} ->
            stats
            { rs2xx = stepStatistics (rs2xx stats) resultTime
            , rsRollup = stepStatistics (rsRollup stats) resultTime
            }
        ErrorStatus {resultTime, errorCode}
            | is4xx errorCode ->
                stats
                { rs4xx = stepStatistics (rs4xx stats) resultTime
                , rsRollup = stepStatistics (rsRollup stats) resultTime
                , rsTestsFailed = rsTestsFailed stats + 1
                }
            | otherwise ->
                stats
                { rs5xx = stepStatistics (rs5xx stats) resultTime
                , rsRollup = stepStatistics (rsRollup stats) resultTime
                , rsTestsFailed = rsTestsFailed stats + 1
                }
        Error {resultTime} ->
            stats
            { rsFailed = stepStatistics (rsFailed stats) resultTime
            , rsRollup = stepStatistics (rsRollup stats) resultTime
            , rsTestsFailed = rsTestsFailed stats + 1
            }
        RuntimeError -> stats {rsTestsFailed = rsTestsFailed stats + 1}
        End -> stats {rsTotalTests = rsTotalTests stats + 1}

count2xx :: ResultStatistics -> Int
count2xx = statsCount . rs2xx

count4xx :: ResultStatistics -> Int
count4xx = statsCount . rs4xx

count5xx :: ResultStatistics -> Int
count5xx = statsCount . rs5xx

countFailed :: ResultStatistics -> Int
countFailed = statsCount . rsFailed

errorRate :: ResultStatistics -> Double
errorRate x =
    fromIntegral (count4xx x + count5xx x + countFailed x) /
    fromIntegral (count2xx x + count4xx x + count5xx x + countFailed x)

{- | AllStats has all of the ... stats. This type stores all of the information
     'wrecker' uses to display metrics to the user.
-}
data AllStats = AllStats
    { aRollup :: !ResultStatistics
    -- ^ The "total" stats. This computes things like total 2xx and average time
    --   Across all requests.
    , aCompleteRuns :: !ResultStatistics
    -- ^ This contains statistic for actions that completed entirely successfully.
    --   Useful for knowing if a complex action is under some desired total time.
    , aRuns :: !(HashMap Int ResultStatistics)
    -- ^ This is an intermediate holding spot for scripts that are still executing.
    , aPerUrl :: !(HashMap String ResultStatistics)
    -- ^ This is the per key (URL) statistics.
    } deriving (Show)

emptyAllStats :: AllStats
emptyAllStats =
    AllStats
    { aRollup = emptyResultStatistics
    , aCompleteRuns = emptyResultStatistics
    , aRuns = H.empty
    , aPerUrl = H.empty
    }

is4xx :: Int -> Bool
is4xx x = x > 399 && x < 500

stepAllStats :: AllStats -> Int -> String -> RunResult -> AllStats
stepAllStats allStats index key result =
    case result of
        End ->
            let mRunStats = H.lookup index $ aRuns allStats
            in case mRunStats of
                   Nothing -> allStats
                   Just stats
                       | rsTestsFailed stats == 0 && errorRate stats == 0 ->
                           let runTime = sTotal $ rs2xx stats
                           in allStats
                              { aCompleteRuns =
                                    stepResultStatistics
                                        (aCompleteRuns allStats)
                                        (Success runTime "")
                              , aRuns = H.delete index $ aRuns allStats
                              , aRollup = stepResultStatistics (aRollup allStats) result
                              }
                       | otherwise ->
                           allStats
                           { aRollup = stepResultStatistics (aRollup allStats) result
                           , aRuns = H.delete index $ aRuns allStats
                           }
        RuntimeError ->
            allStats
            { aRollup = stepResultStatistics (aRollup allStats) result
            , aRuns =
                  H.insertWith
                      (\_ x -> stepResultStatistics x result)
                      index
                      (stepResultStatistics emptyResultStatistics result) $
                  aRuns allStats
            }
        _ ->
            allStats
            { aRollup = stepResultStatistics (aRollup allStats) result
            , aRuns =
                  H.insertWith
                      (\_ x -> stepResultStatistics x result)
                      index
                      (stepResultStatistics emptyResultStatistics result) $
                  aRuns allStats
            , aPerUrl =
                  H.insertWith
                      (\_ x -> stepResultStatistics x result)
                      key
                      (stepResultStatistics emptyResultStatistics result) $
                  aPerUrl allStats
            }

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------
statToRow :: ResultStatistics -> [String]
statToRow x =
    [ printf "%.4f" $ mean $ rs2xx x
    , fixNaN (quantile95 $ rs2xx x)
    , fixBounds (maximumValue $ rs2xx x)
    , fixBounds (minimumValue $ rs2xx x)
    , show $ count2xx x
    , show $ count4xx x
    , show $ count5xx x
    , show $ countFailed x
    , fixNaN (errorRate x)
    ]
  where
    fixNaN n =
        if isNaN n
            then "N/A"
            else printf "%.4f" n
    fixBounds n =
        if isInfinite n
            then "N/A"
            else printf "%.4f" n

pprStats :: Maybe Int -> URLDisplay -> AllStats -> String
pprStats nameSize urlDisplay stats =
    let totals = AsciiArt.render id id id $ totalsTable stats
        urlsTable = AsciiArt.render id id id $ statsTable nameSize urlDisplay stats
    in urlsTable ++ "\n\n" ++ totals

totalsTable :: AllStats -> Table String String String
totalsTable AllStats {..} =
    Table
        (Group NoLine [Header "Test Runs"])
        (Group SingleLine [Header "Total", Header "Failed"])
        [[show $ rsTotalTests aRollup, show $ rsTestsFailed aRollup]]

adjustKey :: Maybe Int -> URLDisplay -> String -> String
adjustKey keySize urlDisplay key =
    maybe id take keySize $
    case urlDisplay of
        Path -> urlToPathPieceKey key
        Full -> key

statsTable :: Maybe Int -> URLDisplay -> AllStats -> Table String String String
statsTable urlSize urlDisp AllStats {..} =
    let sortedPerUrl = sortBy (compare `on` fst) $ H.toList aPerUrl
    in Table
           (Group SingleLine $ map (Header . adjustKey urlSize urlDisp . fst) sortedPerUrl)
           (Group
                SingleLine
                [ Header "mean"
                , Header "95%"
                , Header "max"
                , Header "min"
                , Header "2xx"
                , Header "4xx"
                , Header "5xx"
                , Header "Failures"
                , Header "Error Rate"
                ])
           (map (statToRow . snd) sortedPerUrl) +====+
       SemiTable (Group SingleLine [Header "All"]) (statToRow aRollup) +====+
       SemiTable (Group SingleLine [Header "Successful Runs"]) (statToRow aCompleteRuns)

printStats :: Options -> AllStats -> IO ()
printStats options sampler =
    putStrLn $ pprStats (requestNameColumnSize options) (urlDisplay options) sampler

------------------------------------------------------------------------------
-- JSON Serialization
------------------------------------------------------------------------------
instance ToJSON Statistics where
    toJSON x =
        object
            [ "mean" .= mean x
            , "quantile95" .= fixNaN (quantile95 x)
            , "variance" .= fixNaN (variance x)
            , "max" .= fixBounds (maximumValue x)
            , "min" .= fixBounds (minimumValue x)
            , "total" .= sTotal x
            , "count" .= statsCount x
            ]
      where
        fixBounds n =
            if isInfinite n
                then 0
                else n
        fixNaN n =
            if isNaN n
                then 0
                else n

instance ToJSON ResultStatistics where
    toJSON ResultStatistics {..} =
        object
            [ "2xx" .= rs2xx
            , "4xx" .= rs4xx
            , "5xx" .= rs5xx
            , "failed" .= rsFailed
            , "rollup" .= rsRollup
            ]

instance ToJSON AllStats where
    toJSON AllStats {..} =
        object
            [ "per-request" .=
              Object (H.fromList $ map (\(k, v) -> (T.pack k, toJSON v)) $ H.toList aPerUrl)
            , "runs" .= aCompleteRuns
            , "rollup" .= aRollup
            , "totalRuns" .= rsTotalTests aRollup
            , "totalFailures" .= rsTestsFailed aRollup
            ]
