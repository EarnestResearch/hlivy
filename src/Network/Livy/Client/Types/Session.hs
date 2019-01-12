{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Livy.Client.Types.Session where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Typeable


data SessionKind
  = SparkSession
  | PySparkSession
  | SparkRSession
  | SQLSession
    deriving (Eq, Typeable)

instance Show SessionKind where
  show SparkSession   = "spark"
  show PySparkSession = "pyspark"
  show SparkRSession  = "sparkr"
  show SQLSession     = "sql"

instance ToJSON SessionKind where
  toJSON = String . pack . show

instance FromJSON SessionKind where
  parseJSON = withText "SessionKind" $ \case
    "spark"   -> return SparkSession
    "pyspark" -> return PySparkSession
    "sparkr"  -> return SparkRSession
    "sql"     -> return SQLSession
    t         -> fail . unpack $ "Unknown session type: " <> t


data SessionState
  = SessionNotStarted
  | SessionStarting
  | SessionIdle
  | SessionBusy
  | SessionShuttingDown
  | SessionError
  | SessionDead
  | SessionSuccess
    deriving (Eq, Typeable)

instance Show SessionState where
  show SessionNotStarted   = "not_started"
  show SessionStarting     = "starting"
  show SessionIdle         = "idle"
  show SessionBusy         = "busy"
  show SessionShuttingDown = "shutting_down"
  show SessionError        = "error"
  show SessionDead         = "dead"
  show SessionSuccess      = "success"

instance ToJSON SessionState where
  toJSON = String . pack . show

instance FromJSON SessionState where
  parseJSON = withText "SessionState" $ \case
    "not_started"   -> return SessionNotStarted
    "starting"      -> return SessionStarting
    "idle"          -> return SessionIdle
    "busy"          -> return SessionBusy
    "shutting_down" -> return SessionShuttingDown
    "error"         -> return SessionError
    "dead"          -> return SessionDead
    "success"       -> return SessionSuccess
    s               -> fail . unpack $ "Unknown session state: " <> s


data Session = Session
  { _sId        :: !Int
  , _sAppId     :: !Text
  , _sOwner     :: !Text
  , _sProxyUser :: !Text
  , _sKind      :: !SessionKind
  , _sLog       :: ![Text]
  , _sState     :: !SessionState
  , _sAppInfo   :: !Object
  } deriving (Eq, Show, Typeable)

makeLenses ''Session

instance ToJSON Session where
  toJSON s = object
    [ "id"        .= (s ^. sId)
    , "appId"     .= (s ^. sAppId)
    , "owner"     .= (s ^. sOwner)
    , "proxyUser" .= (s ^. sProxyUser)
    , "kind"      .= (s ^. sKind)
    , "log"       .= (s ^. sLog)
    , "state"     .= (s ^. sState)
    , "appInfo"   .= (s ^. sAppInfo)
    ]

instance FromJSON Session where
  parseJSON = withObject "Session" $ \o -> Session
    <$> o .: "id"
    <*> o .: "appId"
    <*> o .: "owner"
    <*> o .: "proxyUser"
    <*> o .: "kind"
    <*> o .: "log"
    <*> o .: "state"
    <*> o .: "appInfo"
