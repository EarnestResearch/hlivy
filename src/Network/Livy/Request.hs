{-|
Module      :  Network.Livy.Request
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Request
  ( -- * Creating HTTP requests
    get
  , getQuery
  , post
  , postBody
  , postJSON
  , delete
    -- * Modifying requests
  , setHost
  , setPort
  ) where


import           Control.Lens ((&))
import           Data.Aeson
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client
import           Network.HTTP.Types

import           Network.Livy.Types


-- | Basic GET request.
get :: ToPath a => a -> Request
get x = defaultRequest { path = toPath x }


-- | GET request with a collection of query parameters.
getQuery :: (ToPath a, ToQuery a) => a -> Request
getQuery x = get x & setQueryString (toQueryString x)


-- | Basic POST request.
post :: ToPath a => a -> Request
post x = (get x) { method = "POST" }


-- | POST request with a given request body specified as key-value pairs.
postBody
  :: ToPath a
  =>  a -- ^ The basic request.
  -> [(Text, Text)] -- ^ The desired request body.
  -> Request
postBody x kv = (post x)
  { requestHeaders = [(hContentType, "application/json")]
  , requestBody    = formatRequestBody kv
  }


-- | POST request with a 'RequestBody' derived from an instance of 'ToJSON'.
postJSON :: (ToPath a, ToJSON a) => a -> Request
postJSON x = (post x)
  { requestHeaders = [(hContentType, "application/json")]
  , requestBody    = RequestBodyLBS $ encode x
  }


-- | Basic DELETE request.
delete :: ToPath a => a -> Request
delete x = (get x) { method = "DELETE" }


-- | Set the hostname in the request.
setHost :: S.ByteString -> Request -> Request
setHost h req = req { host = h }


-- | Set the port in the request.
setPort :: Int -> Request -> Request
setPort p req = req { port = p }


-- | Make a JSON 'RequestBody' from a list of key-value pairs.
formatRequestBody :: [(Text, Text)] -> RequestBody
formatRequestBody = RequestBodyLBS . encode . Map.fromList . filter (not . T.null . snd)
