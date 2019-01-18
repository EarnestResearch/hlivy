{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Batch.GetBatch
  ( -- * The request
    GetBatch (..)
  , getBatch
    -- ** Request lenses
  , gbBatchId
    -- * The response
  , GetBatchResponse (..)
    -- ** Response lenses
  , gbrBatch
  ) where

import Control.Lens
import Data.Aeson.TH
import Data.Typeable

import Network.Livy.Client.Internal.JSON
import Network.Livy.Client.Types.Batch
import Network.Livy.Internal.Text
import Network.Livy.Request
import Network.Livy.Types


-- | The 'GetBatch' request object.
newtype GetBatch = GetBatch
  { _gbBatchId :: BatchId -- ^ Id of the batch session.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetBatch

instance ToPath GetBatch where
  toPath r = toPath ["batches", toText $ r ^. gbBatchId]

instance LivyRequest GetBatch where
  request = get


-- | Creates a value of 'GetBatch' with the minimum fields required to make a request.
getBatch :: BatchId -> GetBatch
getBatch = GetBatch


-- | The 'GetBatch' response body.
newtype GetBatchResponse = GetBatchResponse
  { _gbrBatch :: Batch -- ^ The 'Batch' object.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetBatchResponse
deriveFromJSON ((recordPrefixOptions 3) { unwrapUnaryRecords = True }) ''GetBatchResponse
type instance LivyResponse GetBatch = GetBatchResponse
