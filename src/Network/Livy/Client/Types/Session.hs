{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Livy.Client.Types.Session
  ( -- * Interactive sessions.
    Session (..)
  , SessionKind (..)
  , SessionState (..)
    -- ** Lenses.
  , sId
  , sAppId
  , sOwner
  , sProxyUser
  , sKind
  , sLog
  , sState
  , sAppInfo
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON


-- | The kind of Livy session.
data SessionKind
  = SparkSession -- ^ A Scala Spark session.
  | PySparkSession -- ^ A PySpark session.
  | SparkRSession -- ^ A SparkR session.
  | SQLSession -- ^ A Spark SQL session.
    deriving (Eq, Show, Typeable)

instance ToJSON SessionKind where
  toJSON = String . T.toLower . T.reverse . T.drop 7 . T.reverse . T.pack . show

instance FromJSON SessionKind where
  parseJSON = withText "SessionKind" $ \case
    "spark"   -> return SparkSession
    "pyspark" -> return PySparkSession
    "sparkr"  -> return SparkRSession
    "sql"     -> return SQLSession
    t         -> fail . T.unpack $ "Unknown session type: " <> t


-- ^ The present state of a session.
data SessionState
  = SessionNotStarted -- ^ Session has not been started.
  | SessionStarting -- ^ Session is starting.
  | SessionIdle -- ^ Session is waiting for input.
  | SessionBusy -- ^ Session is executing a statement.
  | SessionShuttingDown -- ^ Session is shutting down.
  | SessionError -- ^ Session errored out.
  | SessionDead -- ^ Session has exited.
  | SessionSuccess -- ^ Session is successfully stopped.
    deriving (Eq, Show, Typeable)

instance ToJSON SessionState where
  toJSON SessionNotStarted   = String "not_started"
  toJSON SessionStarting     = String "starting"
  toJSON SessionIdle         = String "idle"
  toJSON SessionBusy         = String "busy"
  toJSON SessionShuttingDown = String "shutting_down"
  toJSON SessionError        = String "error"
  toJSON SessionDead         = String "dead"
  toJSON SessionSuccess      = String "success"

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
    s               -> fail . T.unpack $ "Unknown session state: " <> s


-- ^ An interactive session with Livy.
data Session = Session
  { _sId        :: !Int -- ^ The session id.
  , _sAppId     :: !Text -- ^ The application id of this session.
  , _sOwner     :: !Text -- ^ Remote user who submitted this session.
  , _sProxyUser :: !Text -- ^ User to impersonate when running.
  , _sKind      :: !SessionKind -- ^ Session kind (spark, pyspark, sparkr, sql).
  , _sLog       :: ![Text] -- ^ The log lines.
  , _sState     :: !SessionState -- ^ The session state.
  , _sAppInfo   :: !Object -- ^ The detailed application info.
  } deriving (Eq, Show, Typeable)

makeLenses ''Session
deriveJSON (recordPrefixOptions 2) ''Session
