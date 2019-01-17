{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.GetSession
  ( -- * The request.
    GetSession (..)
  , getSession
    -- ** Request lenses.
  , gsSessionId
    -- * The response.
  , GetSessionResponse (..)
    -- ** Response lenses.
  , gsSession
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Session
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'GetSession' request object.
newtype GetSession = GetSession
  { _gsSessionId :: Int -- ^ Id of the session.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSession

instance ToPath GetSession where
  toPath r = C.pack $ "/sessions/" <> show (r ^. gsSessionId)

instance LivyRequest GetSession where
  request = get


-- | Creates a value of 'GetSessions' with the minimum fields required to make a request.
getSession :: Int -> GetSession
getSession = GetSession


-- | The 'GetSession' response body.
newtype GetSessionResponse = GetSessionResponse
  { _gsSession :: Session -- ^ The 'Session'.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionResponse
deriveFromJSON (recordPrefixOptions 3) ''GetSessionResponse
type instance LivyResponse GetSession = GetSessionResponse
