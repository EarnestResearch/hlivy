{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module      :  Network.Livy.Client.Batch.GetBatches
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Client.Batch.GetBatches
  ( -- * The request
    GetBatches (..)
  , getBatches
   -- ** Request lenses
  , gbsFrom
  , gbsSize
    -- * The response
  , GetBatchesResponse (..)
    -- ** Response lenses
  , gbsrFrom
  , gbsrSize
  , gbsrSessions
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Maybe (isJust)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Batch
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'GetBatches' request object.
data GetBatches = GetBatches
  { _gbsFrom :: Maybe Int -- ^ The start index to fetch batches.
  , _gbsSize :: Maybe Int -- ^ Number of batches to fetch.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetBatches

instance ToPath GetBatches where
  toPath = const "batches"

instance ToQuery GetBatches where
  toQueryString r = filter (isJust . snd)
              [ ("from", C.pack . show <$> r ^. gbsFrom)
              , ("size", C.pack . show <$> r ^. gbsSize)
              ]

instance LivyRequest GetBatches where
  request = getQuery


-- | Creates a value of 'GetBatches' with the minimum fields required to make a request.
getBatches :: GetBatches
getBatches = GetBatches Nothing Nothing


data GetBatchesResponse = GetBatchesResponse
  { _gbsrFrom     :: !Int -- ^ The start index to fetch batches.
  , _gbsrSize     :: !(Maybe Int) -- ^ Number of batches to fetch.
  , _gbsrSessions :: ![Batch] -- ^ 'Batch' list.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetBatchesResponse
deriveFromJSON (recordPrefixOptions 5) ''GetBatchesResponse
type instance LivyResponse GetBatches = GetBatchesResponse
