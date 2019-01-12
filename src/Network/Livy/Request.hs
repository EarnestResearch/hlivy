module Network.Livy.Request where


import           Control.Lens ((&))
import           Data.Aeson
import qualified Data.ByteString as S
import           Network.HTTP.Client
import           Network.Livy.Types


-- | Set the hostname in the request.
setHost :: S.ByteString -> Request -> Request
setHost h req = req { host = h }


-- | Set the port in the request.
setPort :: Int -> Request -> Request
setPort p req = req { port = p }


-- | Basic GET request.
get :: ToPath a => a -> Request
get x = defaultRequest { path = toPath x }


-- | GET request with a collection of query parameters.
getQuery :: (ToPath a, ToQuery a) => a -> Request
getQuery x = get x & setQueryString (toQuery x)


-- | Basic POST request.
post :: ToPath a => a -> Request
post x = (get x) { method = "POST" }


-- | POST request with a 'RequestBody'.
postBody :: (ToPath a, ToRequestBody a) => a -> Request
postBody x = (post x) { requestBody = toRequestBody x }


-- | POST request with a 'RequestBody' derived from an instance of 'ToJSON'.
postJSON :: (ToPath a, ToJSON a) => a -> Request
postJSON x = (post x) { requestBody = RequestBodyLBS $ encode x }


-- | Basic DELETE request.
delete :: ToPath a => a -> Request
delete x = (get x) { method = "DELETE" }
