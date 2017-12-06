{-| This is a copy of the 'wreq' 'Session', but each call is wrapped in
    'wrecker''s 'record' function.

    This file was initially copied from Network.Wreq.Session
    (c) 2014 Bryan O'Sullivan. See the source for the full copy right info.
-}
-- All of this code below was copied from bos's `Network.Wreq.Session`
-- and modified to include the wrecker recorder
{-# LANGUAGE RecordWildCards, RankNTypes #-}

module Network.Wreq.Wrecker
    ( Session
    , defaultManagerSettings
    , withRecordFunction
    -- * Session Creation
    , withWreq
    , withWreqNoCookies
    , withWreqSettings
    -- * HTTP Methods
    , get
    , post
    , head_
    , options
    , put
    , delete
    -- * HTTP Methods with Options
    , getWith
    , postWith
    , headWith
    , optionsWith
    , putWith
    , deleteWith
    -- * HTTP Methods to get JSON responses
    , getJSON
    , getJSONWith
    , postJSON
    , postJSONWith
    , putJSON
    , putJSONWith
    , deleteJSON
    , deleteJSONWith
    ) where

import Control.Exception (fromException, handle, throwIO)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy as L
import Data.Default (def)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Encoding
import Network.Connection (ConnectionContext)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as Session
import qualified Network.Wreq.Types as Wreq
import Wrecker

{-| An opaque type created by 'withWreq', 'withWreqNoCookies',
    or 'withWreqSettings'. All HTTP calls require a 'Session'.
-}
data Session = Session
    { sSession :: Session.Session
    , sRecorder :: Recorder
    , sRecord :: forall a. Recorder -> String -> IO a -> IO a
      -- ^ A custom function to record the time of of executing the IO action
      --   By default, it will use 'Wrecker.Recorder.record'
    }

{- | Create 'ManagerSettings' with no timeout using a shared TLS
     'ConnectionContext'
-}
defaultManagerSettings :: ConnectionContext -> HTTP.ManagerSettings
defaultManagerSettings context =
    (TLS.mkManagerSettingsContext (Just context) def Nothing)
    {HTTP.managerResponseTimeout = HTTP.responseTimeoutNone}

-- | Create a 'Session' using the 'wrecker' 'Environment', passing it to the
--   given function.  The 'Session' will no longer be valid after that
--   function returns.
--
-- This session manages cookies and uses default session manager
-- configuration.
withWreq :: (Session -> IO a) -> Environment -> IO a
withWreq f env =
    withWreqSettings
        (recorder env)
        (Just (HTTP.createCookieJar []))
        (defaultManagerSettings (context env))
        f

-- | Create a session.
--
-- This uses the default session manager settings, but does not manage
-- cookies.  It is intended for use with REST-like HTTP-based APIs,
-- which typically do not use cookies.
withWreqNoCookies :: (Session -> IO a) -> Environment -> IO a
withWreqNoCookies f env =
    withWreqSettings (recorder env) Nothing (defaultManagerSettings (context env)) f

-- | Replaces the record function of the Session with the provided one.
--
-- This is useful for custom recorder actions, or if you need to catch any exceptions
-- thrown by the IO action and don't wish them to bubble up to the statistics.
withRecordFunction :: (forall a. Recorder -> String -> IO a -> IO a) -> Session -> Session
withRecordFunction r sess = sess {sRecord = r}

-- | Create a session, using the given cookie jar and manager settings.
withWreqSettings ::
       Recorder
    -> Maybe HTTP.CookieJar
                 -- ^ If 'Nothing' is specified, no cookie management
                 -- will be performed.
    -> HTTP.ManagerSettings
    -> (Session -> IO a)
    -> IO a
withWreqSettings recorder cookie settings f =
    Session.withSessionControl cookie settings $ \session -> f (Session session recorder record)

-- this records things. It's not ideal, but an more acurate
-- implementation is harder. Pull requests welcome.
withRecorder :: (Session.Session -> String -> IO a) -> Session -> String -> IO a
withRecorder f (Session {..}) key = sRecord sRecorder key $ f sSession key

withRecorder1 :: (Session.Session -> String -> a -> IO b) -> Session -> String -> a -> IO b
withRecorder1 f (Session {..}) key b = sRecord sRecorder key $ f sSession key b

-- | 'Session'-specific version of 'Network.Wreq.get'.
get :: Session -> String -> IO (HTTP.Response L.ByteString)
get = withRecorder Session.get

-- | 'Session'-specific version of 'Network.Wreq.post'.
post :: Wreq.Postable a => Session -> String -> a -> IO (HTTP.Response L.ByteString)
post = withRecorder1 Session.post

-- | 'Session'-specific version of 'Network.Wreq.head_'.
head_ :: Session -> String -> IO (HTTP.Response ())
head_ = withRecorder Session.head_

-- | 'Session'-specific version of 'Network.Wreq.options'.
options :: Session -> String -> IO (HTTP.Response ())
options = withRecorder Session.options

-- | 'Session'-specific version of 'Network.Wreq.put'.
put :: Wreq.Putable a => Session -> String -> a -> IO (HTTP.Response L.ByteString)
put = withRecorder1 Session.put

-- | 'Session'-specific version of 'Network.Wreq.delete'.
delete :: Session -> String -> IO (HTTP.Response L.ByteString)
delete = withRecorder Session.delete

-- | 'Session'-specific version of 'Network.Wreq.getWith'.
getWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response L.ByteString)
getWith opts = withRecorder (Session.getWith opts)

-- | 'Session'-specific version of 'Network.Wreq.postWith'.
postWith ::
       Wreq.Postable a => Wreq.Options -> Session -> String -> a -> IO (HTTP.Response L.ByteString)
postWith opts = withRecorder1 (Session.postWith opts)

-- | 'Session'-specific version of 'Network.Wreq.headWith'.
headWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response ())
headWith opts = withRecorder (Session.headWith opts)

-- | 'Session'-specific version of 'Network.Wreq.optionsWith'.
optionsWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response ())
optionsWith opts = withRecorder (Session.optionsWith opts)

-- | 'Session'-specific version of 'Network.Wreq.putWith'.
putWith ::
       Wreq.Putable a => Wreq.Options -> Session -> String -> a -> IO (HTTP.Response L.ByteString)
putWith opts = withRecorder1 (Session.putWith opts)

-- | 'Session'-specific version of 'Network.Wreq.deleteWith'.
deleteWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response L.ByteString)
deleteWith opts = withRecorder (Session.deleteWith opts)

-- | 'Session'-specific version of 'Network.Wreq.get' that expects a JSON response.
getJSON :: FromJSON a => Session -> String -> IO (HTTP.Response a)
getJSON = withRecorder (\sess url -> Session.get sess url >>= fromJSON "GET" url)

-- | 'Session'-specific version of 'Network.Wreq.post' that expects a JSON response.
postJSON :: (Wreq.Postable a, FromJSON b) => Session -> String -> a -> IO (HTTP.Response b)
postJSON = withRecorder1 (\sess url body -> Session.post sess url body >>= fromJSON "POST" url)

-- | 'Session'-specific version of 'Network.Wreq.put' that expects a JSON response.
putJSON :: (Wreq.Putable a, FromJSON b) => Session -> String -> a -> IO (HTTP.Response b)
putJSON = withRecorder1 (\sess url body -> Session.put sess url body >>= fromJSON "PUT" url)

-- | 'Session'-specific version of 'Network.Wreq.delete' that expects a JSON response.
deleteJSON :: FromJSON a => Session -> String -> IO (HTTP.Response a)
deleteJSON = withRecorder (\sess url -> Session.delete sess url >>= fromJSON "DELETE" url)

-- | 'Session'-specific version of 'Network.Wreq.getWith' that expects a JSON response.
getJSONWith :: FromJSON a => Wreq.Options -> Session -> String -> IO (HTTP.Response a)
getJSONWith opts = withRecorder (\sess url -> Session.getWith opts sess url >>= fromJSON "GET" url)

-- | 'Session'-specific version of 'Network.Wreq.postWith' that expects a JSON response.
postJSONWith ::
       (Wreq.Postable a, FromJSON b)
    => Wreq.Options
    -> Session
    -> String
    -> a
    -> IO (HTTP.Response b)
postJSONWith opts =
    withRecorder1 (\sess url body -> Session.postWith opts sess url body >>= fromJSON "POST" url)

-- | 'Session'-specific version of 'Network.Wreq.putWith' that expects a JSON response.
putJSONWith ::
       (Wreq.Putable a, FromJSON b)
    => Wreq.Options
    -> Session
    -> String
    -> a
    -> IO (HTTP.Response b)
putJSONWith opts =
    withRecorder1 (\sess url body -> Session.putWith opts sess url body >>= fromJSON "PUT" url)

-- | 'Session'-specific version of 'Network.Wreq.deleteWith' that expects a JSON response.
deleteJSONWith :: FromJSON a => Wreq.Options -> Session -> String -> IO (HTTP.Response a)
deleteJSONWith opts =
    withRecorder (\sess url -> Session.deleteWith opts sess url >>= fromJSON "DELETE" url)

-- | Helper function used to create better error messages when failing to decode JSON responses
fromJSON :: FromJSON a => String -> String -> HTTP.Response L.ByteString -> IO (HTTP.Response a)
fromJSON verb url response = handle decorateEx (Wreq.asJSON response)
  where
    decorateEx ex =
        case fromException ex of
            Just (Wreq.JSONError err) ->
                throwIO
                    (LogicError
                         ("Error decoding the JSON response from " ++
                          verb ++
                          " " ++ url ++ " : " ++ err ++ "\n\n" ++ "Actually got:\n" ++ responseBody))
            _ -> throwIO ex
    responseBody :: String
    responseBody = Text.unpack . Encoding.decodeUtf8 . HTTP.responseBody $ response
