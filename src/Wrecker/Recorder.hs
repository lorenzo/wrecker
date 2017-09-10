{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Wrecker.Recorder where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import System.Clock
import System.Clock.TimeIt

data RunResult
    = Success { resultTime :: !Double
              , name :: !String }
    | ErrorStatus { resultTime :: !Double
                  , errorCode :: !Int
                  , name :: !String }
    | Error { resultTime :: !Double
            , exception :: !SomeException
            , name :: !String }
    | End
    deriving (Show)

data Event = Event
    { eRunIndex :: !Int
    , eResult :: !RunResult
    } deriving (Show)

-- | An opaque type for recording actions for profiling.
--   To obtain a 'Recorder' use either 'run', 'defaultMain', 'runOne' or
--   'newStandaloneRecorder'.
data Recorder = Recorder
    { rWithQuery :: !Bool
    , rRunIndex :: !Int
    , rQueue :: !(TBMQueue Event)
    }

newtype LogicError =
    LogicError String

instance Show LogicError where
    show (LogicError err) = err

instance Exception LogicError

newtype HandledError =
    HandledError SomeException
    deriving (Show)

instance Exception HandledError

-- The bound here should be configurable
split :: Recorder -> Recorder
split Recorder {..} = Recorder rWithQuery (rRunIndex + 1) rQueue

newRecorder :: Bool -> Int -> IO Recorder
newRecorder withQuery maxSize = Recorder withQuery 0 <$> newTBMQueueIO maxSize

stopRecorder :: Recorder -> IO ()
stopRecorder = atomically . closeTBMQueue . rQueue

addEvent :: Recorder -> RunResult -> IO ()
addEvent (Recorder _ runIndex queue) runResult =
    atomically $ writeTBMQueue queue $ Event runIndex runResult

readEvent :: Recorder -> IO (Maybe Event)
readEvent = atomically . readTBMQueue . rQueue

{- | 'record' is a low level function for collecting timing information.
     Wrap each action of interest in a call to record.

> record recorder $ threadDelay 1000000

  'record' measures the elapsed time of the call, and catches
  'HttpException' in the case of failure. This means failures
   must be thrown if they are to be properly recorded.
-}
record :: forall a. Recorder -> String -> IO a -> IO a
record recorder key action = do
    !startTime <- getTime Monotonic
    handle (recordException startTime) (recordAction startTime)
  where
    cleanKey =
        if rWithQuery recorder
            then key
            else takeWhile (/= '?') key -- Remove the query string
    recordAction :: TimeSpec -> IO a
    recordAction startTime = do
        r <- action
        endTime <- getTime Monotonic
        let !elapsedTime' = diffSeconds endTime startTime
        addEvent recorder $ Success {resultTime = elapsedTime', name = cleanKey}
        return r
    recordException :: TimeSpec -> SomeException -> IO a
    recordException startTime ex = do
        endTime <- getTime Monotonic
        handleException endTime ex
        throwIO (HandledError ex)
      where
        handleException endTime e
            | Just ((HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _))) <-
                 fromException e = do
                let code = HTTP.statusCode $ HTTP.responseStatus resp
                addEvent recorder $
                    ErrorStatus
                    {resultTime = diffSeconds endTime startTime, errorCode = code, name = cleanKey}
            | otherwise =
                addEvent recorder $
                Error
                { resultTime = diffSeconds endTime startTime
                , exception = toException e
                , name = cleanKey
                }
