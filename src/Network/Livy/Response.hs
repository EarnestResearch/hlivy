{-# LANGUAGE FlexibleContexts #-}

module Network.Livy.Response
  ( -- * Receiving a response
    send
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import           Network.HTTP.Client
import           Network.HTTP.Types

import           Network.Livy.Env
import           Network.Livy.Monad
import           Network.Livy.Request
import           Network.Livy.Types


-- | Send a request, returning the associated response if successful.
send :: LivyConstraint r m a => a -> m (Either LivyError (LivyResponse a))
send req = reader (view environment) >>= handleResponse req


-- | Send a 'LivyRequest' and handle the response.
handleResponse
  :: (MonadIO m, LivyRequest b, FromJSON a)
  => b -- ^ The 'LivyRequest'.
  -> Env -- ^ An 'Env' to perform the request in.
  -> m (Either LivyError a)
handleResponse req env = do
  let req' = request req & setHost (env ^. envHost) & setPort (env ^. envPort)
  resp <- liftIO $ httpLbs req' (env ^. envManager)
  let rs = responseStatus resp
  if livyError rs then return $
      makeLivyHTTPError (statusCode rs) (statusMessage rs) (responseBody resp)
  else return $ parseResponse resp


-- | Parse the Livy response into a 'LivyResponse', if possible.
parseResponse :: FromJSON a => Response LBS.ByteString -> Either LivyError a
parseResponse resp = case eitherDecode' (responseBody resp) of
  Left e  -> Left $ LivyError ParseFailure (C.pack e) Nothing Nothing
  Right v -> Right v


-- | Whether there was an error resulting from the request.
livyError :: Status -> Bool
livyError s = statusCode s >= 400


-- | Create a 'LivyError' value for an HTTP error returned from Livy.
makeLivyHTTPError
  :: Int -- ^ The HTTP status code.
  -> S.ByteString -- ^ Status message.
  -> LBS.ByteString -- ^ Response body.
  -> Either LivyError a
makeLivyHTTPError c m b = Left $ LivyError InvalidRequest m (Just b) $
  case c of
    400 -> Just BadRequest
    401 -> Just Unauthorized
    402 -> Just RequestFailed
    403 -> Just Forbidden
    404 -> Just NotFound
    405 -> Just BadMethod
    500 -> Just ServerError
    501 -> Just ServerError
    502 -> Just ServerError
    503 -> Just ServerError
    504 -> Just ServerError
    _   -> Just UnknownHTTPErrorCode
