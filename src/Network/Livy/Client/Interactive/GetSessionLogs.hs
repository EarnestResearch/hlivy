{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.GetSessionLogs
  ( -- * The request
    GetSessionLogs (..)
  , getSessionLogs
    -- ** Request lenses
  , gslFrom
  , gslSize
    -- * The response
  , GetSessionLogsResponse (..)
    -- ** Response lenses
  , gslrId
  , gslrFrom
  , gslrSize
  , gslrLog
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Session
import           Network.Livy.Internal.Text
import           Network.Livy.Request
import           Network.Livy.Types


-- ^ The 'GetSessionLogs' request object.
data GetSessionLogs = GetSessionLogs
  { _gslSessionId :: SessionId -- ^ Id of the session.
  , _gslFrom      :: Maybe Int -- ^ Offset.
  , _gslSize      :: Maybe Int -- ^ Max number of log lines to return.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionLogs

instance ToPath GetSessionLogs where
  toPath r = toPath ["sessions", toText $ r ^. gslSessionId, "log"]

instance ToQuery GetSessionLogs where
  toQueryString r = filter (isJust . snd)
    [ ("from", C.pack . show <$> r ^. gslFrom)
    , ("size", C.pack . show <$> r ^. gslSize)
    ]

instance LivyRequest GetSessionLogs where
  request = getQuery


-- | Creates a value of 'GetSessionLogs' with the minimum fields required to make a request.
getSessionLogs :: SessionId -> GetSessionLogs
getSessionLogs n = GetSessionLogs n Nothing Nothing


-- | The 'GetSessionLogs' response body.
data GetSessionLogsResponse = GetSessionLogsResponse
  { _gslrId   :: !Int -- ^ The session id.
  , _gslrFrom :: !Int -- ^ Offset from start of log.
  , _gslrSize :: !(Maybe Int) -- ^ Max number of log lines.
  , _gslrLog  :: ![Text] -- ^ The log lines.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionLogsResponse
deriveFromJSON (recordPrefixOptions 5) ''GetSessionLogsResponse
type instance LivyResponse GetSessionLogs = GetSessionLogsResponse
