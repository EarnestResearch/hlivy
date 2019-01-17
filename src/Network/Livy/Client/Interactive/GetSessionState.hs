{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.GetSessionState
  ( -- * The request.
    GetSessionState (..)
  , getSessionState
    -- ** Request lenses.
  , gsstSessionId
    -- * The response.
  , GetSessionStateResponse (..)
    -- ** Response lenses.
  , gsstrSessionId
  , gsstrState
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Session
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'GetSessionState' request object.
newtype GetSessionState = GetSessionState
  { _gsstSessionId :: Int -- ^ Id of the session.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionState

instance ToPath GetSessionState where
  toPath r = C.pack $ "/sessions/" <> show (r ^. gsstSessionId) <> "/state"

instance LivyRequest GetSessionState where
  request = get


-- | Creates a value of 'GetSessionState' with the minimum fields required to make a request.
getSessionState :: Int -> GetSessionState
getSessionState = GetSessionState


-- | The 'GetSessionState' response body.
data GetSessionStateResponse = GetSessionStateResponse
  { _gsstrSessionId :: !(Maybe Int) -- ^ Session id.
  , _gsstrState     :: !(Maybe SessionState) -- ^ The current state of the session.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionStateResponse
deriveFromJSON (recordPrefixOptions 6) ''GetSessionStateResponse
type instance LivyResponse GetSessionState = GetSessionStateResponse
