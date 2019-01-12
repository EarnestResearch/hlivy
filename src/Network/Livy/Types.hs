{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Types where

import           Control.Exception
import           Control.Lens
import qualified Data.ByteString as S
import           Data.Typeable
import           Network.HTTP.Client
import           Network.HTTP.Types


-- | Specify how a value is converted to a URL fragment.
class ToPath a where
  toPath :: a -> S.ByteString


-- | Specify how a value is converted to a collection of query parameters.
class ToQuery a where
  toQuery :: a -> Query


-- | Specify how a value is converted to a request body.
class ToRequestBody a where
  toRequestBody :: a -> RequestBody


-- | Specify how a request is created.
class LivyRequest a where
  request :: a -> Request


-- | The response type of a Livy request.
type family LivyResponse a :: *


-- | Environment required to make requests to Livy.
data Env = Env
  { _envManager :: !Manager
  , _envHost    :: !S.ByteString
  , _envPort    :: !Int
  }

makeLenses ''Env


class HasEnv a where
  hasEnv :: a -> Env


instance HasEnv Env where
  hasEnv = id


-- | Livy error types.
data LivyErrorType
  = InvalidRequest
  | APIError
  | ConnectionError
  | ParseFailure
  | UnknownErrorType
    deriving (Show, Typeable)


-- | HTTP response error codes.
data LivyHTTPErrorCode
  = BadRequest
  | Unauthorized
  | RequestFailed
  | Forbidden
  | NotFound
  | ServerError
  | UnknownHTTPCode
    deriving (Show, Typeable)


-- | An error with Livy.
data LivyError = LivyError
  { leType    :: !LivyErrorType
  , leMessage :: !S.ByteString
  , leCode    :: !(Maybe LivyHTTPErrorCode)
  } deriving (Show, Typeable)

instance Exception LivyError where
