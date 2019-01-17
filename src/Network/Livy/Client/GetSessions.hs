{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.GetSessions where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Typeable
import           GHC.Generics (Generic)

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Session
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'GetSessions' request object.
data GetSessions = GetSessions
  { _gssFrom :: Maybe Int
  , _gssSize :: Maybe Int
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessions

instance ToPath GetSessions where
  toPath = const "/sessions"

instance ToQuery GetSessions where
  toQuery req = [ ("from", C.pack . show <$> req ^. gssFrom)
                , ("size", C.pack . show <$> req ^. gssSize)
                ]

instance LivyRequest GetSessions where
  request = getQuery


-- | Creates a value of 'GetSessions' with the minimum fields required to make a request.
getSessions :: GetSessions
getSessions = GetSessions Nothing Nothing


-- | The 'GetSessions' response body.
data GetSessionsResponse = GetSessionsResponse
  { _gssrFrom     :: !Int
  , _gssrSize     :: !Int
  , _gssrSessions :: ![Session]
  } deriving (Eq, Show, Generic)

makeLenses ''GetSessionsResponse
deriveFromJSON (recordPrefixOptions 5) ''GetSessionsResponse
type instance LivyResponse GetSessions = GetSessionsResponse
