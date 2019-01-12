{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.GetSessionState where

import           Control.Lens hiding ((.=))
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import           Data.Typeable

import           Network.Livy.Client.Types.Session
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'GetSessionState' request object.
newtype GetSessionState = GetSessionState { _gsstSessionId :: Int } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionState

instance ToPath GetSessionState where
  toPath s = C.pack $ "/sessions/" <> show (s ^. gsstSessionId) <> "/state"

instance LivyRequest GetSessionState where
  request = get


-- | Creates a value of 'GetSessionState' with the minimum fields required to make a request.
getSessionState :: Int -> GetSessionState
getSessionState = GetSessionState


-- | The 'GetSessionState' response body.
data GetSessionStateResponse = GetSessionStateResponse
  { _gsstrSessionId :: !(Maybe Int)
  , _gsstrState     :: !(Maybe SessionState)
  } deriving (Eq, Show, Typeable)

instance FromJSON GetSessionStateResponse where
  parseJSON = withObject "GetSessionStateResponse" $ \o -> GetSessionStateResponse
    <$> o .: "id"
    <*> o .: "state"

type instance LivyResponse GetSessionState = GetSessionStateResponse
