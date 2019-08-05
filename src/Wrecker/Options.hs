{-# LANGUAGE RecordWildCards #-}

module Wrecker.Options where

import Control.Exception
import Data.Monoid
import Data.Semigroup (Semigroup(..))
import Options.Applicative
import Wrecker.Logger

{- | There are two typical ways to invoke 'wrecker'. 'RunCount' will execute
     each a script 'n' times on each thread. So a run count of 100 and a
     concurrency of 10 will run the script a total of 1000 times.
     Alternatively, 'wrecker' can run for specified number of seconds
     with 'RunTimed'.
-}
data RunType
    = RunCount Int
    | RunTimed Int
    deriving (Show, Eq)

{- | 'DisplayMode' controls how results are displayed in the console. The
default is 'NonInterative' which returns the final results at the end of the
program. 'Interactive' will show partial results as the program updates.
-}
data DisplayMode
    = Interactive
    | NonInteractive
    deriving (Show, Eq, Read)

data URLDisplay
    = Path
    | Full
    deriving (Show, Eq, Read)

data Options = Options
    { concurrency :: Int
      -- ^ The number of simulatanous connections
    , binCount :: Int
      -- ^ The number of bins for the histogram
    , runStyle :: RunType
      -- ^ runStyle determines if the 'wrecker' runs for a specified
      --   time period or for a specified number of runs.
    , timeoutTime :: Int
      -- ^ How long to wait after the first benchmark for the other threads
      --   to finish
    , displayMode :: DisplayMode
      -- ^ This controls the command line display. It can be either Interactive
      --   of NonInteractive
    , logLevel :: LogLevel
      -- ^ The log severity to shown in stderr
    , logFmt :: LogFormat
      -- ^ The log format to use in stderr (either Json or PlainText)
    , match :: String
      -- ^ Set this to filter the benchmarks using a pattern
    , requestNameColumnSize :: Maybe Int
      -- ^ Limit the request name column to the given size
    , outputFilePath :: Maybe FilePath
      -- ^ Dump the results to JSON file
    , silent :: Bool
      -- ^ Set 'silent' to true to disable all output.
    , urlDisplay :: URLDisplay
    -- ^ Whether to display short of full URLs in the report
    , recordQuery :: Bool
      -- ^ Set 'recordQuery' to consider the query string as a different URL
    , listTestGroups :: Bool
    -- ^ Show the list of exposed test groups, if any
    } deriving (Show, Eq)

-- | 'defaultOptions' provides sensible default for the 'Options'
--   types
defaultOptions :: Options
defaultOptions =
    Options
    { concurrency = 10
    , binCount = 20
    , runStyle = RunTimed 10
    , timeoutTime = 100000000
    , displayMode = NonInteractive
    , logLevel = LevelError
    , logFmt = Json
    , match = ""
    , requestNameColumnSize = Nothing
    , outputFilePath = Nothing
    , silent = False
    , urlDisplay = Path
    , recordQuery = False
    , listTestGroups = False
    }

data PartialOptions = PartialOptions
    { mConcurrency :: Maybe Int
    , mBinCount :: Maybe Int
    , mRunStyle :: Maybe RunType
    , mTimeoutTime :: Maybe Int
    , mDisplayMode :: Maybe DisplayMode
    , mLogLevel :: Maybe LogLevel
    , mLogFmt :: Maybe LogFormat
    , mMatch :: Maybe String
    , mRequestNameColumnSize :: Maybe Int
    , mOutputFilePath :: Maybe FilePath
    , mSilent :: Maybe Bool
    , murlDisplay :: Maybe URLDisplay
    , mRecordQuery :: Maybe Bool
    , mListTestGroups :: Maybe Bool
    } deriving (Show, Eq)

instance Semigroup PartialOptions where
    x <> y =
        PartialOptions
        { mConcurrency = mConcurrency x <|> mConcurrency y
        , mBinCount = mBinCount x <|> mBinCount y
        , mRunStyle = mRunStyle x <|> mRunStyle y
        , mTimeoutTime = mTimeoutTime x <|> mTimeoutTime y
        , mDisplayMode = mDisplayMode x <|> mDisplayMode y
        , mLogLevel = mLogLevel x <|> mLogLevel y
        , mLogFmt = mLogFmt x <|> mLogFmt y
        , mMatch = mMatch x <|> mMatch y
        , mRequestNameColumnSize = mRequestNameColumnSize x <|> mRequestNameColumnSize y
        , mOutputFilePath = mOutputFilePath x <|> mOutputFilePath y
        , mSilent = mSilent x <|> mSilent y
        , murlDisplay = murlDisplay x <|> murlDisplay y
        , mRecordQuery = mRecordQuery x <|> mRecordQuery y
        , mListTestGroups = mListTestGroups x <|> mListTestGroups y
        }

instance Monoid PartialOptions where
    mempty =
        PartialOptions
        { mConcurrency = Just $ concurrency defaultOptions
        , mBinCount = Just $ binCount defaultOptions
        , mRunStyle = Just $ runStyle defaultOptions
        , mTimeoutTime = Just $ timeoutTime defaultOptions
        , mDisplayMode = Just $ displayMode defaultOptions
        , mLogLevel = Just $ logLevel defaultOptions
        , mLogFmt = Just $ logFmt defaultOptions
        , mMatch = Just $ match defaultOptions
        , mRequestNameColumnSize = requestNameColumnSize defaultOptions
        , mOutputFilePath = outputFilePath defaultOptions
        , mSilent = Just $ silent defaultOptions
        , murlDisplay = Just $ urlDisplay defaultOptions
        , mRecordQuery = Just $ recordQuery defaultOptions
        , mListTestGroups = Just $ listTestGroups defaultOptions
        }
    mappend = (<>)

completeOptions :: PartialOptions -> Maybe Options
completeOptions options =
    case options <> mempty of
        PartialOptions { mConcurrency = Just concurrency
                       , mBinCount = Just binCount
                       , mRunStyle = Just runStyle
                       , mTimeoutTime = Just timeoutTime
                       , mDisplayMode = Just displayMode
                       , mLogLevel = Just logLevel
                       , mLogFmt = Just logFmt
                       , mMatch = Just match
                       , mRequestNameColumnSize = requestNameColumnSize
                       , mOutputFilePath = outputFilePath
                       , mSilent = Just silent
                       , murlDisplay = Just urlDisplay
                       , mRecordQuery = Just recordQuery
                       , mListTestGroups = Just listTestGroups
                       } -> Just Options {..}
        _ -> Nothing

optionalOption :: Read a => Mod OptionFields a -> Parser (Maybe a)
optionalOption = optional . option auto

optionalStrOption :: Mod OptionFields String -> Parser (Maybe String)
optionalStrOption = optional . strOption

optionalSwitch :: Mod FlagFields Bool -> Parser (Maybe Bool)
optionalSwitch = optional . switch

pPartialOptions :: Parser PartialOptions
pPartialOptions =
    PartialOptions <$>
    optionalOption (long "concurrency" <> help "Number of threads for concurrent requests") <*>
    --
    optionalOption (long "bin-count" <> help "Number of bins for latency histogram") <*>
    --
    optional
        (RunCount <$> option auto (long "run-count" <> help "number of times to repeat") <|>
         RunTimed <$> option auto (long "run-timed" <> help "number of seconds to repeat")) <*>
    --
    optionalOption (long "timeout-time" <> help "How long to wait for all requests to finish") <*>
    --
    optional
        (NonInteractive <$ switch (long "non-interactive") <|>
         Interactive <$ switch (long "interactive")) <*>
    --
    optionalOption
        (long "log-level" <>
         help
             "Log to stderr events of criticality greater than the LOG_LEVEL (LevelWarn | LevelError | LevelInfo | LevelDebug)") <*>
    --
    optionalOption (long "log-format" <> help "Log format to use (Json | PlainText)") <*>
    --
    optionalStrOption (long "match" <> help "Only run tests that match the glob") <*>
    --
    optionalOption (long "request-name-size" <> help "Request name size for the terminal display") <*>
    --
    optionalStrOption
        (long "output-path" <> help "Save a JSON file of the the statistics to given path") <*>
    optionalSwitch (long "silent" <> help "Disable all output") <*>
    --
    optional
        (Path <$ switch (long "relative-url-display") <|>
         Full <$ switch (long "absolute-url-display")) <*>
    --
    optionalSwitch
        (long "record-query" <> help "Take in consideration the query string for the report") <*>
    --
    optionalSwitch (long "list-test-groups" <> help "Shows the list of tests to run and exit")

{- | Run the command line parse and return the 'Options'
-}
runParser :: IO Options
runParser = do
    let opts =
            info
                (helper <*> pPartialOptions)
                (fullDesc <> progDesc "Welcome to wrecker" <>
                 header "wrecker - HTTP stress tester and benchmarker")
    partialOptions <- execParser opts
    case completeOptions partialOptions of
        Nothing -> throwIO $ userError ""
        Just x -> return x
