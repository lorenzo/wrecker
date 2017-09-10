{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
  This module can be used to describe the operations that can be performed
  against an external API that uses JSON as its serialization format.

  It exposes functions tailored to get and post data in the JSON format and
  to unserialize the responses into types that can be converted form JSON.

  Use the 'Ref' and 'RPC' types to describe the external API operations.
-}
module Network.Wreq.Wrecker.API
    ( Ref(..)
    , RPC(..)
    -- * Interacting with an external API
    , get
    , post
    , put
    , rpc
    , delete
    -- * Network functions with Options
    , getWith
    , postWith
    , putWith
    , rpcWith
    , deleteWith
    -- * Utility functions
    , query
    ) where

import Data.Aeson (FromJSON(..), ToJSON, encode, withText)
import Data.ByteString.Lazy as LB (ByteString)
import Data.Text (Text, unpack)
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.URI as URI
import qualified Network.Wreq as Wreq
import Network.Wreq.Types (Postable(..), Putable)

import qualified Network.Wreq.Wrecker as Wrecker

{-|
  A Ref represents an API resource whose contents can be retrieved with GET

  We use the phantom type parameter `response` so that we can use
  this information for understanding what type of data the API
  resource will return

  For example, an API endpoint returning a list of User is modelled as follows:

  > usersRef :: Ref [User]
  > usersRef = Ref "http://localhost:8080/users"
-}
data Ref response = Ref
    { unRef :: Text
    } deriving (Show, Eq)

{-|
  A Req represents an API resource whose contents can be retrieved with POST

  We use the phantom type parameter `response` so that we can use
  this information for understanding what type of data the API
  resource will return

  Similarly, the request parameter is used to know what type of data can be posted
  to the endpoint.

  For example, an API endpoint that receives a User and returns a list of Friendship is
  modelled as follows:

  > newFriendRpc :: User -> RPC [Friendship] User
  > newFriendRpc User { userID } = RPC "http://localhost:8080/users/" <> userID <> "/friendships"
-}
newtype RPC response request = RPC
    { rpcUrl :: Text
    } deriving (Show, Eq)

instance FromJSON (Ref a) where
    parseJSON = withText "FromJSON (Ref a)" (return . Ref)

instance FromJSON (RPC a b) where
    parseJSON = withText "FromJSON (Ref a)" (return . RPC)

--
-- Network functions
--
{-|
   Gets the results form an API endpoint that responds in the JSON format.

   > users <- get sess usersRef
   > mapM_ (print . userID) users
-}
get :: FromJSON a => Wrecker.Session -> Ref a -> IO a
get sess (Ref url) = fmap HTTP.responseBody (Wrecker.getJSON sess (unpack url))

{-|
   Gets the results form an API endpoint that responds in the JSON format.
   This function excepts additional options, such as custom headers

   > let opts = Network.Wreq.defaults & Network.Wreq.auth ?~ Network.Wreq.basicAuth "user" "pass"
   > users <- getWith opts sess usersRef
   > mapM_ (print . userID) users
-}
getWith :: FromJSON a => Wreq.Options -> Wrecker.Session -> Ref a -> IO a
getWith opts sess (Ref url) = fmap HTTP.responseBody (Wrecker.getJSONWith opts sess (unpack url))

{-|
   Posts some data to an API endpoint in any 'Network.Wreq.Types.Postable' format
   and gets the response in JSON format.

   > Friendships friends <- post sess (newFriendRpc user) user2
   > mapM_ (print . userID) friends
-}
post ::
       (Postable req, FromJSON res)
    => Wrecker.Session
    -> RPC res req
    -> req
    -- ^ The request payload
    -> IO res
post sess (RPC url) body = fmap HTTP.responseBody (Wrecker.postJSON sess (unpack url) body)

{-|
   Posts some data to an API endpoint in any 'Network.Wreq.Types.Postable' format
   and gets the response in JSON format.
   This function excepts additional options, such as custom headers

   > let opts = Network.Wreq.defaults & Network.Wreq.auth ?~ Network.Wreq.basicAuth "user" "pass"
   > Friendships friends <- post sess (newFriendRpc user) user2
   > mapM_ (print . userID) friends
-}
postWith ::
       (Postable req, FromJSON res)
    => Wreq.Options
    -> Wrecker.Session
    -> RPC res req
    -> req
    -- ^ The request payload
    -> IO res
postWith opts sess (RPC url) body =
    fmap HTTP.responseBody (Wrecker.postJSONWith opts sess (unpack url) body)

{-|
   Puts some data to an API endpoint in any 'Network.Wreq.Types.Putable' format
   and gets the response in JSON format.

   > Friendships friends <- put sess (newFriendRpc user) user2
   > mapM_ (print . userID) friends
-}
put :: (Putable req, FromJSON res)
    => Wrecker.Session
    -> RPC res req
    -> req
    -- ^ The request payload
    -> IO res
put sess (RPC url) body = fmap HTTP.responseBody (Wrecker.putJSON sess (unpack url) body)

{-|
   Puts some data to an API endpoint in any 'Network.Wreq.Types.Putable' format
   and gets the response in JSON format.
   This function excepts additional options, such as custom headers

   > let opts = Network.Wreq.defaults & Network.Wreq.auth ?~ Network.Wreq.basicAuth "user" "pass"
   > Friendships friends <- put sess (newFriendRpc user) user2
   > mapM_ (print . userID) friends
-}
putWith ::
       (Putable req, FromJSON res)
    => Wreq.Options
    -> Wrecker.Session
    -> RPC res req
    -> req
    -- ^ The request payload
    -> IO res
putWith opts sess (RPC url) body =
    fmap HTTP.responseBody (Wrecker.putJSONWith opts sess (unpack url) body)

{-|
   Posts some data to an API endpoint in any JSON format
   and gets the response in JSON format.

   > Friendships friends <- post sess (newFriendRpc user) user2
   > mapM_ (print . userID) friends
-}
rpc :: (ToJSON req, FromJSON res)
    => Wrecker.Session
    -> RPC res req
    -> req
    -- ^ The request payload
    -> IO res
rpc sess (RPC url) body = fmap HTTP.responseBody (Wrecker.postJSON sess (unpack url) jsonBody)
  where
    jsonBody = encode body

{-|
   Posts some data to an API endpoint in any JSON format
   and gets the response in JSON format.
   This function excepts additional options, such as custom headers

   > let opts = Network.Wreq.defaults & Network.Wreq.auth ?~ Network.Wreq.basicAuth "user" "pass"
   > Friendships friends <- post sess (newFriendRpc user) user2
   > mapM_ (print . userID) friends
-}
rpcWith ::
       (ToJSON req, FromJSON res)
    => Wreq.Options
    -> Wrecker.Session
    -> RPC res req
    -> req
    -- ^ The request payload
    -> IO res
rpcWith opts sess (RPC url) body =
    fmap HTTP.responseBody (Wrecker.postJSONWith opts sess (unpack url) jsonBody)
  where
    jsonBody = encode body

{-|
   Sends as DELETE request to an API endpoint that responds in the JSON format.

   > Status { success } <- delete sess usersDeleteRef
-}
delete :: FromJSON a => Wrecker.Session -> Ref a -> IO a
delete sess (Ref url) = fmap HTTP.responseBody (Wrecker.deleteJSON sess (unpack url))

{-|
   Sends as DELETE request to an API endpoint that responds in the JSON format.

   > let opts = Network.Wreq.defaults & Network.Wreq.auth ?~ Network.Wreq.basicAuth "user" "pass"
   > Status { success } <- deleteWith opts sess usersDeleteRef
-}
delete :: FromJSON a => Wreq.Options -> Wrecker.Session -> Ref a -> IO a
delete opts sess (Ref url) = fmap HTTP.responseBody (Wrecker.deleteJSONWith opts sess (unpack url))

--
-- Utitily functions
--
query :: [(Text, Text)] -> Text
query parts = T.decodeUtf8 (URI.renderSimpleQuery True converted)
  where
    converted = fmap (\(k, v) -> (T.encodeUtf8 k, T.encodeUtf8 v)) parts

--
-- Orphans
--
{-|
   The form attributes are passed as tuples,
   representing key-value pairs: For example

   > postPayload [("username", "jon"), ("password", "snow")]
-}
instance Postable [(Text, Text)] where
    postPayload p = (postPayload . toFormData) p
      where
        toFormData d = fmap (\(k, v) -> (T.encodeUtf8 k, T.encodeUtf8 v)) d

{-|
   Some API endpoints are odd in that they expect a POST as a verb, but
   no request body is expected. This models such case.
-}
instance Postable () where
    postPayload _ = postPayload ("" :: ByteString)
