{-# LANGUAGE TemplateHaskell #-}

module Network.Livy.Client.Types.Batch
  ( -- * Batch sessions.
    Batch (..)
    -- ** Lenses.
  , bId
  , bAppId
  , bAppInfo
  , bLog
  , bState
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Typeable

import Network.Livy.Client.Internal.JSON


-- | A batch session with Livy.
data Batch = Batch
  { _bId      :: !Int -- ^ The session id.
  , _bAppId   :: !Text -- ^ The application id of this session.
  , _bAppInfo :: !Object -- ^ The detailed application info.
  , _bLog     :: ![Text] -- ^ The log lines.
  , _bState   :: !Text -- ^ The batch state.
  } deriving (Eq, Show, Typeable)

makeLenses ''Batch
deriveJSON (recordPrefixOptions 2) ''Batch
