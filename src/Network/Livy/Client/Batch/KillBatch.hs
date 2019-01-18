{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Batch.KillBatch
  ( -- * The request
    KillBatch (..)
  , killBatch
    -- ** Request lenses
  , kbBatchId
    -- * The response
  , KillBatchResponse (..)
    -- ** Response lenses
  , kbrMsg
  ) where

import Control.Lens
import Data.Aeson.TH
import Data.Text (Text)
import Data.Typeable

import Network.Livy.Client.Internal.JSON
import Network.Livy.Client.Types.Batch
import Network.Livy.Internal.Text
import Network.Livy.Request
import Network.Livy.Types


-- | The 'KillBatch' request object.
newtype KillBatch = KillBatch
  { _kbBatchId :: BatchId -- ^ Id of the batch session.
  } deriving (Eq, Show, Typeable)

makeLenses ''KillBatch

instance ToPath KillBatch where
  toPath s = toPath ["batches", toText $ s ^. kbBatchId]

instance LivyRequest KillBatch where
  request = delete


-- | Creates a value of 'KillBatch' with the minimum fields required to make a request.
killBatch :: BatchId -> KillBatch
killBatch = KillBatch


-- | The 'KillBatch' response body.
newtype KillBatchResponse = KillBatchResponse
  { _kbrMsg :: Maybe Text -- ^ Message from the Livy server.
  } deriving (Eq, Show, Typeable)

makeLenses ''KillBatchResponse
deriveFromJSON (recordPrefixOptions 4) ''KillBatchResponse
type instance LivyResponse KillBatch = KillBatchResponse
