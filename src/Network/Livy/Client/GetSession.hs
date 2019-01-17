{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.GetSession where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Typeable
import           GHC.Generics (Generic)

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Session
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'GetSession' request object.
newtype GetSession = GetSession { _gsSessionId :: Int } deriving (Eq, Show, Typeable)

makeLenses ''GetSession

instance ToPath GetSession where
  toPath s = C.pack $ "/sessions/" <> show (s ^. gsSessionId)

instance LivyRequest GetSession where
  request = get


-- | Creates a value of 'GetSessions' with the minimum fields required to make a request.
getSession :: Int -> GetSession
getSession = GetSession


-- | The 'GetSession' response body.
newtype GetSessionResponse = GetSessionResponse
  { _gsSession :: Session
  } deriving (Eq, Show, Typeable, Generic)

makeLenses ''GetSessionResponse
deriveFromJSON (recordPrefixOptions 3) ''GetSessionResponse
type instance LivyResponse GetSession = GetSessionResponse
