{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module      :  Network.Livy.Client.Batch.GetBatchState
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Client.Batch.GetBatchState
  ( -- * The request
    GetBatchState (..)
  , getBatchState
    -- ** Request lenses
  , gbstBatchId
    -- * The response
  , GetBatchStateResponse (..)
    -- ** Response lenses
  , gbstrId
  , gbstrState
  ) where

import Control.Lens
import Data.Aeson.TH
import Data.Typeable

import Network.Livy.Client.Internal.JSON
import Network.Livy.Client.Types.Batch
import Network.Livy.Internal.Text
import Network.Livy.Request
import Network.Livy.Types


-- | The 'GetBatchState' request object.
newtype GetBatchState = GetBatchState
  { _gbstBatchId :: BatchId -- ^ Id of the batch session.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetBatchState

instance ToPath GetBatchState where
  toPath r = toPath ["batches", toText $ r ^. gbstBatchId, "state"]

instance LivyRequest GetBatchState where
  request = get


-- | Creates a value of 'GetBatchState' with the minimum fields required to make a request.
getBatchState :: BatchId -> GetBatchState
getBatchState = GetBatchState


-- | The 'GetBatchState' response body.
data GetBatchStateResponse = GetBatchStateResponse
  { _gbstrId    :: !BatchId -- ^ Batch id.
  , _gbstrState :: !BatchState -- ^ The current state of the batch session.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetBatchStateResponse
deriveFromJSON (recordPrefixOptions 6) ''GetBatchStateResponse
type instance LivyResponse GetBatchState = GetBatchStateResponse
