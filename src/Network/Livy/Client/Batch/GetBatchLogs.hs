{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Batch.GetBatchLogs
  ( -- * The request
    GetBatchLogs (..)
  , getBatchLogs
    -- ** Request lenses
  , gblFrom
  , gblSize
    -- * The response
  , GetBatchLogsResponse (..)
    -- ** Response lenses
  , gblrId
  , gblrFrom
  , gblrSize
  , gblrLog
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Batch
import           Network.Livy.Internal.Text
import           Network.Livy.Request
import           Network.Livy.Types


-- ^ The 'GetBatchLogs' request object.
data GetBatchLogs = GetBatchLogs
  { _gblBatchId :: BatchId -- ^ Id of the batch session.
  , _gblFrom    :: Maybe Int -- ^ Offset.
  , _gblSize    :: Maybe Int -- ^ Max number of log lines to return.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetBatchLogs

instance ToPath GetBatchLogs where
  toPath r = toPath ["batches", toText $ r ^. gblBatchId, "log"]

instance ToQuery GetBatchLogs where
  toQueryString r = filter (isJust . snd)
              [ ("from", C.pack . show <$> r ^. gblFrom)
              , ("size", C.pack . show <$> r ^. gblSize)
              ]

instance LivyRequest GetBatchLogs where
  request = getQuery


-- | Creates a value of 'GetBatchLogs' with the minimum fields required to make a request.
getBatchLogs :: BatchId -> GetBatchLogs
getBatchLogs bid = GetBatchLogs bid Nothing Nothing


-- | The 'GetBatchLogs' response body.
data GetBatchLogsResponse = GetBatchLogsResponse
  { _gblrId   :: !BatchId -- ^ The batch session id.
  , _gblrFrom :: !Int -- ^ Offset from start of log.
  , _gblrSize :: !(Maybe Int) -- ^ Max number of log lines.
  , _gblrLog  :: ![Text] -- ^ The log lines.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetBatchLogsResponse
deriveFromJSON (recordPrefixOptions 5) ''GetBatchLogsResponse
type instance LivyResponse GetBatchLogs = GetBatchLogsResponse
