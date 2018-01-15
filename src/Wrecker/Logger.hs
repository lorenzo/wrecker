{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wrecker.Logger where

import Control.Monad (when)
import Data.Aeson
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import System.Log.FastLogger

data LogLevel
    = LevelDebug
    | LevelInfo
    | LevelWarn
    | LevelError
    deriving (Show, Eq, Ord, Read)

data LogFormat
    = PlainText
    | Json
    deriving (Eq, Show, Read)

data Logger = Logger
    { currentLevel :: LogLevel
    , logFormat :: LogFormat
    , logFunc :: FastLogger
    , cleanup :: IO ()
    , timeFormatter :: IO FormattedTime
    }

-- | Create a logger using stderr. This is the typical way a logger is created.
newStdErrLogger :: LogLevel -> LogFormat -> IO Logger
newStdErrLogger level format = do
    let timeFormat =
            if format == PlainText
                then "%FT%T"
                else "%s"
    timeCache <- newTimeCache timeFormat
    (logger, clean) <- newFastLogger (LogStderr defaultBufSize)
    return
        Logger
        { currentLevel = level
        , logFormat = format
        , logFunc = logger
        , cleanup = clean
        , timeFormatter = timeCache
        }

-- True if the write was successful or False otherwise
writeLogger :: Logger -> LogLevel -> LogStr -> IO ()
writeLogger Logger {..} messageLevel msg =
    when (currentLevel <= messageLevel) $ do
        t <- timeFormatter
        logFunc (formatMsg logFormat messageLevel t msg)

formatMsg :: LogFormat -> LogLevel -> FormattedTime -> LogStr -> LogStr
formatMsg PlainText level prettyTime msg =
    toLogStr ("[" <> prettyTime <> "] - ") <> formatLevel level <> msg <> "\n"
formatMsg Json level prettyTime msg = toLogStr (encode jsonMsg) <> "\n"
  where
    jsonMsg =
        object
            [ "level" .= toLevelCode level
            , "timestamp" .= TE.decodeUtf8 prettyTime
            , "full_message" .= TE.decodeUtf8 (fromLogStr msg)
            ]

formatLevel :: LogLevel -> LogStr
formatLevel level = "[" <> lvl level <> "] - "
  where
    lvl LevelDebug = "DBUG"
    lvl LevelInfo = "INFO"
    lvl LevelWarn = "WARN"
    lvl LevelError = "ERRO"

toLevelCode :: LogLevel -> Int
toLevelCode LevelDebug = 7
toLevelCode LevelInfo = 6
toLevelCode LevelWarn = 4
toLevelCode LevelError = 3

shutdownLogger :: Logger -> IO ()
shutdownLogger Logger {..} = cleanup

logDebug :: ToLogStr msg => Logger -> msg -> IO ()
logDebug logger = writeLogger logger LevelDebug . toLogStr

logInfo :: ToLogStr msg => Logger -> msg -> IO ()
logInfo logger = writeLogger logger LevelInfo . toLogStr

logWarn :: ToLogStr msg => Logger -> msg -> IO ()
logWarn logger = writeLogger logger LevelWarn . toLogStr

logError :: ToLogStr msg => Logger -> msg -> IO ()
logError logger = writeLogger logger LevelError . toLogStr
