module Network.Livy.Response where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import           Network.HTTP.Client
import           Network.HTTP.Types

import           Network.Livy.Request
import           Network.Livy.Types


-- | Create a request to Livy using a 'Manager' and interpret the response.
parseHttp
  :: (LivyRequest b, FromJSON a)
  => b -- ^ A 'LivyReqest'.
  -> Env -- ^ The environment needed to make the request.
  -> IO (Either LivyError a)
parseHttp req env = do
  let req' = request req & setHost (env ^. envHost) & setPort (env ^. envPort)
  resp <- httpLbs req' (env ^. envManager)
  let rs = responseStatus resp
  if httpError rs
    then return $ makeLivyHttpError (statusCode rs) (statusMessage rs)
    else case eitherDecode' (responseBody resp) of
           Left e  -> return . Left $
             LivyError ParseFailure (C.pack e) Nothing
           Right v -> return $ Right v


-- | Whether there was an error resulting from the request.
httpError :: Status -> Bool
httpError s = statusCode s /= 200


-- | Create a 'LivyError' value for an HTTP error returned from Livy.
makeLivyHttpError
  :: Int
  -> S.ByteString
  -> Either LivyError a
makeLivyHttpError c m = Left $ LivyError InvalidRequest m $
  case c of
    400 -> Just BadRequest
    401 -> Just Unauthorized
    402 -> Just RequestFailed
    404 -> Just NotFound
    500 -> Just ServerError
    502 -> Just ServerError
    503 -> Just ServerError
    504 -> Just ServerError
    _   -> Just UnknownHTTPCode
