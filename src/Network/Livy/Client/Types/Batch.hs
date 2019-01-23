{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Network.Livy.Client.Types.Batch
  ( -- * Batch sessions
    Batch (..)
  , BatchId (..)
  , BatchAppInfo
  , BatchState (..)
    -- ** Lenses
  , bId
  , bAppId
  , bAppInfo
  , bLog
  , bState
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Internal.Text


-- | The id of this batch session.
newtype BatchId = BatchId Int
  deriving (Eq, Show, Typeable, ToText, ToJSON, FromJSON)


-- | Detailed application information.
type BatchAppInfo = Map.HashMap Text (Maybe Text)


-- | The present state of a batch session.
data BatchState
  = BatchNotStarted -- ^ Batch session has not been started.
  | BatchStarting -- ^ Batch session is starting.
  | BatchRecovering -- ^ Batch session is recovering.
  | BatchIdle -- ^ Batch session is waiting for input.
  | BatchRunning -- ^ Batch session is running.
  | BatchBusy -- ^ Batch session is executing a statement.
  | BatchShuttingDown -- ^ Batch session is shutting down.
  | BatchError -- ^ Batch session errored out.
  | BatchDead -- ^ Batch session has exited.
  | BatchKilled -- ^ Batch session is killed.
  | BatchSuccess -- ^ Batch session is successfully stopped.
    deriving (Bounded, Enum, Eq, Show, Typeable)

instance ToText BatchState where
  toText BatchNotStarted   = "not_started"
  toText BatchStarting     = "starting"
  toText BatchRecovering   = "recovering"
  toText BatchIdle         = "idle"
  toText BatchRunning      = "running"
  toText BatchBusy         = "busy"
  toText BatchShuttingDown = "shutting_down"
  toText BatchError        = "error"
  toText BatchDead         = "dead"
  toText BatchKilled       = "killed"
  toText BatchSuccess      = "success"

instance ToJSON BatchState where
  toJSON = String . toText

instance FromJSON BatchState where
  parseJSON = withText "BatchState" $ \t ->
    case lookup t toTextLookup of
      Just st -> return st
      Nothing -> fail . T.unpack $ "Unknown batch state: " <> t


-- | A batch session with Livy.
data Batch = Batch
  { _bId      :: !BatchId -- ^ The session id.
  , _bAppId   :: !(Maybe Text) -- ^ The application id of this session.
  , _bAppInfo :: !BatchAppInfo -- ^ The detailed application info.
  , _bLog     :: ![Text] -- ^ The log lines.
  , _bState   :: !BatchState -- ^ The batch state.
  } deriving (Eq, Show, Typeable)

makeLenses ''Batch
deriveJSON (recordPrefixOptions 2) ''Batch
