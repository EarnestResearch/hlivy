{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module      :  Network.Livy.Types
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Types
  ( -- * Request/response types
    LivyRequest (..)
  , LivyResponse
  , ToPath (..)
  , ToQuery (..)
    -- * Exceptions
  , LivyError (..)
  , LivyErrorType (..)
  , LivyHTTPErrorCode (..)
    -- ** Lenses
  , leCode
  , leMessage
  , leResponseBody
  , leType
  ) where

import           Control.Exception
import           Control.Lens
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Network.HTTP.Client
import           Network.HTTP.Types


-- | Specify how a value is converted to a URL fragment.
class ToPath a where
  toPath :: a -> S.ByteString

instance ToPath Text where toPath = T.encodeUtf8
instance ToPath a => ToPath [a] where toPath = S.intercalate "/" . fmap toPath


-- | Specify how a value is converted to a collection of query parameters.
class ToQuery a where
  toQueryString :: a -> Query


-- | Specify how a request is created.
class LivyRequest a where
  request :: a -> Request


-- | The response type of a Livy request.
type family LivyResponse a :: *


-- | Livy error types.
data LivyErrorType
  = InvalidRequest
  | ParseFailure
  | LibraryException HttpException
  | UnknownErrorType
    deriving (Show, Typeable)


-- | HTTP response error codes.
data LivyHTTPErrorCode
  = BadRequest
  | Unauthorized
  | RequestFailed
  | Forbidden
  | BadMethod
  | NotFound
  | ServerError
  | UnknownHTTPErrorCode
    deriving (Show, Typeable)


-- | An error with Livy.
data LivyError = LivyError
  { _leType         :: !LivyErrorType
  , _leMessage      :: !S.ByteString
  , _leResponseBody :: !(Maybe LBS.ByteString)
  , _leCode         :: !(Maybe LivyHTTPErrorCode)
  } deriving (Show, Typeable)

makeLenses ''LivyError

instance Exception LivyError where
