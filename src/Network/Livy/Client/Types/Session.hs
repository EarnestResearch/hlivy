{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|
Module      :  Network.Livy.Client.Types.Session
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Client.Types.Session
  ( -- * Interactive sessions
    Session (..)
  , SessionId (..)
  , SessionKind (..)
  , SessionState (..)
  , SessionAppInfo
    -- ** Lenses
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
import qualified Data.HashMap.Strict as Map
#if ! MIN_VERSION_base(4,8,0)
import           Data.Monoid (Monoid (..))
#endif
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Internal.Text


-- | The kind of Livy session.
data SessionKind
  = SparkSession -- ^ A Scala Spark session.
  | PySparkSession -- ^ A PySpark session.
  | SparkRSession -- ^ A SparkR session.
  | SQLSession -- ^ A Spark SQL session.
  | SharedSession -- ^ A session that supports all types.
    deriving (Bounded, Enum, Eq, Show, Typeable)

instance ToText SessionKind where
  toText = T.toLower . T.reverse . T.drop 7 . T.reverse . T.pack . show

instance ToJSON SessionKind where
  toJSON = String . toText

instance FromJSON SessionKind where
  parseJSON = withText "SessionKind" $ \t ->
    case lookup t toTextLookup of
      Just sk -> return sk
      Nothing -> fail . T.unpack $ "Unknown session type: " <> t


-- | The present state of a session.
data SessionState
  = SessionNotStarted -- ^ Session has not been started.
  | SessionStarting -- ^ Session is starting.
  | SessionRecovering -- ^ Session is recovering.
  | SessionIdle -- ^ Session is waiting for input.
  | SessionRunning -- ^ Session is running.
  | SessionBusy -- ^ Session is executing a statement.
  | SessionShuttingDown -- ^ Session is shutting down.
  | SessionError -- ^ Session errored out.
  | SessionDead -- ^ Session has exited.
  | SessionKilled -- ^ Session is killed.
  | SessionSuccess -- ^ Session is successfully stopped.
    deriving (Bounded, Enum, Eq, Show, Typeable)

instance ToText SessionState where
  toText SessionNotStarted   = "not_started"
  toText SessionStarting     = "starting"
  toText SessionRecovering   = "recovering"
  toText SessionIdle         = "idle"
  toText SessionRunning      = "running"
  toText SessionBusy         = "busy"
  toText SessionShuttingDown = "shutting_down"
  toText SessionError        = "error"
  toText SessionDead         = "dead"
  toText SessionKilled       = "killed"
  toText SessionSuccess      = "success"

instance ToJSON SessionState where
  toJSON = String . toText

instance FromJSON SessionState where
  parseJSON = withText "SessionState" $ \t ->
    case lookup t toTextLookup of
      Just st -> return st
      Nothing -> fail . T.unpack $ "Unknown session state: " <> t


-- | The id of this interactive session.
newtype SessionId = SessionId Int
  deriving (Eq, Show, Typeable, ToText, ToJSON, FromJSON)


-- | Detailed application information.
type SessionAppInfo = Map.HashMap Text (Maybe Text)


-- | An interactive session with Livy.
data Session = Session
  { _sId        :: !SessionId -- ^ The session id.
  , _sAppId     :: !(Maybe Text) -- ^ The application id of this session.
  , _sOwner     :: !(Maybe Text) -- ^ Remote user who submitted this session.
  , _sProxyUser :: !(Maybe Text) -- ^ User to impersonate when running.
  , _sKind      :: !SessionKind -- ^ Session kind (spark, pyspark, sparkr, sql, shared).
  , _sLog       :: ![Text] -- ^ The log lines.
  , _sState     :: !SessionState -- ^ The session state.
  , _sAppInfo   :: !SessionAppInfo -- ^ The detailed application info.
  } deriving (Eq, Show, Typeable)

makeLenses ''Session
deriveJSON (recordPrefixOptions 2) ''Session
