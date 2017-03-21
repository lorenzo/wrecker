{-| This is a copy of the 'wreq' 'Session', but each call is wrapped in
    'wrecker''s 'record' function.

    This file was initially copied from Network.Wreq.Session
    (c) 2014 Bryan O'Sullivan. See the source for the full copy right info.
-}
-- All of this code below was copied from bos's `Network.Wreq.Session`
-- and modified to include the wrecker recorder
{-# LANGUAGE CPP, RecordWildCards #-}

module Network.Wreq.Wrecker
  ( Session
  , defaultManagerSettings
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
  ) where

import qualified Data.ByteString.Lazy as L
import Data.Default (def)
import Network.Connection (ConnectionContext)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.Wreq.Session as Session
import qualified Network.Wreq.Types as Wreq
import Wrecker

{-| An opaque type created by 'withWreq', 'withWreqNoCookies',
    or 'withWreqSettings'. All HTTP calls require a 'Session'.
-}
data Session = Session
  { sSession :: Session.Session
  , sRecorder :: Recorder
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

-- | Create a session, using the given cookie jar and manager settings.
withWreqSettings
  :: Recorder
  -> Maybe HTTP.CookieJar
                 -- ^ If 'Nothing' is specified, no cookie management
                 -- will be performed.
  -> HTTP.ManagerSettings
  -> (Session -> IO a)
  -> IO a
withWreqSettings recorder cookie settings f =
  Session.withSessionControl cookie settings $ \session -> f (Session session recorder)

-- this records things. It's not ideal, but an more acurate
-- implementation is harder. Pull requests welcome.
withSess :: (Session.Session -> String -> IO a) -> Session -> String -> IO a
withSess f sess key = record (sRecorder sess) key $ f (sSession sess) key

withSess1 :: (Session.Session -> String -> a -> IO b) -> Session -> String -> a -> IO b
withSess1 f sess key b = record (sRecorder sess) key $ f (sSession sess) key b

-- | 'Session'-specific version of 'Network.Wreq.get'.
get :: Session -> String -> IO (HTTP.Response L.ByteString)
get = withSess Session.get

-- | 'Session'-specific version of 'Network.Wreq.post'.
post
  :: Wreq.Postable a
  => Session -> String -> a -> IO (HTTP.Response L.ByteString)
post = withSess1 Session.post

-- | 'Session'-specific version of 'Network.Wreq.head_'.
head_ :: Session -> String -> IO (HTTP.Response ())
head_ = withSess Session.head_

-- | 'Session'-specific version of 'Network.Wreq.options'.
options :: Session -> String -> IO (HTTP.Response ())
options = withSess Session.options

-- | 'Session'-specific version of 'Network.Wreq.put'.
put
  :: Wreq.Putable a
  => Session -> String -> a -> IO (HTTP.Response L.ByteString)
put = withSess1 Session.put

-- | 'Session'-specific version of 'Network.Wreq.delete'.
delete :: Session -> String -> IO (HTTP.Response L.ByteString)
delete = withSess Session.delete

-- | 'Session'-specific version of 'Network.Wreq.getWith'.
getWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response L.ByteString)
getWith opts = withSess (Session.getWith opts)

-- | 'Session'-specific version of 'Network.Wreq.postWith'.
postWith
  :: Wreq.Postable a
  => Wreq.Options -> Session -> String -> a -> IO (HTTP.Response L.ByteString)
postWith opts = withSess1 (Session.postWith opts)

-- | 'Session'-specific version of 'Network.Wreq.headWith'.
headWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response ())
headWith opts = withSess (Session.headWith opts)

-- | 'Session'-specific version of 'Network.Wreq.optionsWith'.
optionsWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response ())
optionsWith opts = withSess (Session.optionsWith opts)

-- | 'Session'-specific version of 'Network.Wreq.putWith'.
putWith
  :: Wreq.Putable a
  => Wreq.Options -> Session -> String -> a -> IO (HTTP.Response L.ByteString)
putWith opts = withSess1 (Session.putWith opts)

-- | 'Session'-specific version of 'Network.Wreq.deleteWith'.
deleteWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response L.ByteString)
deleteWith opts = withSess (Session.deleteWith opts)
