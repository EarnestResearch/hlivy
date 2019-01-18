{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.GetSessions
  ( -- * The request
    GetSessions (..)
  , getSessions
    -- ** Request lenses
  , gssFrom
  , gssSize
    -- * The response
  , GetSessionsResponse (..)
    -- ** Response lenses
  , gssrFrom
  , gssrSize
  , gssrSessions
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Maybe (isJust)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Session
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'GetSessions' request object.
data GetSessions = GetSessions
  { _gssFrom :: Maybe Int -- ^ The start index to fetch sessions.
  , _gssSize :: Maybe Int -- ^ Number of sessions to fetch.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessions

instance ToPath GetSessions where
  toPath = const "sessions"

instance ToQuery GetSessions where
  toQueryString r = filter (isJust . snd)
    [ ("from", C.pack . show <$> r ^. gssFrom)
    , ("size", C.pack . show <$> r ^. gssSize)
    ]

instance LivyRequest GetSessions where
  request = getQuery


-- | Creates a value of 'GetSessions' with the minimum fields required to make a request.
getSessions :: GetSessions
getSessions = GetSessions Nothing Nothing


-- | The 'GetSessions' response body.
data GetSessionsResponse = GetSessionsResponse
  { _gssrFrom     :: !Int -- ^ The start index to fetch sessions.
  , _gssrSize     :: !(Maybe Int) -- ^ Number of sessions to fetch.
  , _gssrSessions :: ![Session] -- ^ 'Session' list.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionsResponse
deriveFromJSON (recordPrefixOptions 5) ''GetSessionsResponse
type instance LivyResponse GetSessions = GetSessionsResponse
