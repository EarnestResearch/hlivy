{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Batch.CreateBatch
  ( -- * The request
    CreateBatch (..)
  , createBatch
    -- ** Request lenses
  , cbFile
  , cbProxyUser
  , cbClassName
  , cbArgs
  , cbJars
  , cbPyFiles
  , cbFiles
  , cbDriverMemory
  , cbDriverCores
  , cbExecutorMemory
  , cbExecutorCores
  , cbNumExecutors
  , cbArchives
  , cbQueue
  , cbName
  , cbConf
  , cbHeartbeatTimeoutInSecond
    -- * The response
  , CreateBatchResponse (..)
    -- ** Response lenses
  , cbrBatch
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Batch
import           Network.Livy.Request
import           Network.Livy.Types


-- | Spark configuration properties.
type SparkConf = Map.HashMap Text Text


-- | The 'CreateBatch' request object.
data CreateBatch = CreateBatch
  { _cbFile                     :: Text -- ^ File containing the application to execute.
  , _cbProxyUser                :: Maybe Text -- ^ User to impersonate when running the job.
  , _cbClassName                :: Maybe Text -- ^ Application main class.
  , _cbArgs                     :: Maybe [Text] -- ^ Command line arguments for the application.
  , _cbJars                     :: Maybe [Text] -- ^ Jars to be used in this batch session.
  , _cbPyFiles                  :: Maybe [Text] -- ^ Python files to be used in this batch session.
  , _cbFiles                    :: Maybe [Text] -- ^ Files to be used in this batch session.
  , _cbDriverMemory             :: Maybe Text -- ^ Amount of memory to use for the driver process.
  , _cbDriverCores              :: Maybe Int -- ^ Number of cores to use for the driver process.
  , _cbExecutorMemory           :: Maybe Text -- ^ Amount of memory to use per executor process.
  , _cbExecutorCores            :: Maybe Int -- ^ Number of cores to use for each executor.
  , _cbNumExecutors             :: Maybe Int -- ^ Number of executors to launch for this batch session.
  , _cbArchives                 :: Maybe [Text] -- ^ Archives to be used in this batch session.
  , _cbQueue                    :: Maybe Text  -- ^ The name of the YARN queue submitted to.
  , _cbName                     :: Maybe Text -- ^ The name of this batch session.
  , _cbConf                     :: Maybe SparkConf -- ^ Spark configuration properties.
  , _cbHeartbeatTimeoutInSecond :: Maybe Int -- ^ Timout in seconds after which to orphan the batch session.
  } deriving (Eq, Show, Typeable)

makeLenses ''CreateBatch
deriveToJSON ((recordPrefixOptions 3) { omitNothingFields = True }) ''CreateBatch

instance ToPath CreateBatch where
  toPath = const "batches"

instance LivyRequest CreateBatch where
  request = postJSON


-- | Creates a value of 'CreateBatch' with the minimum fields required to make a request.
createBatch :: Text -> CreateBatch
createBatch f = CreateBatch
  { _cbFile = f
  , _cbProxyUser = Nothing
  , _cbClassName = Nothing
  , _cbArgs = Nothing
  , _cbJars = Nothing
  , _cbPyFiles = Nothing
  , _cbFiles = Nothing
  , _cbDriverMemory = Nothing
  , _cbDriverCores = Nothing
  , _cbExecutorMemory = Nothing
  , _cbExecutorCores = Nothing
  , _cbNumExecutors = Nothing
  , _cbArchives = Nothing
  , _cbQueue = Nothing
  , _cbName = Nothing
  , _cbConf = Nothing
  , _cbHeartbeatTimeoutInSecond = Nothing
  }


-- | The 'CreateBatch' response body.
newtype CreateBatchResponse = CreateBatchResponse
  { _cbrBatch :: Batch -- ^ The created 'Batch'.
  } deriving (Eq, Show, Typeable)

makeLenses ''CreateBatchResponse
deriveFromJSON (defaultOptions { unwrapUnaryRecords = True }) ''CreateBatchResponse
type instance LivyResponse CreateBatch = CreateBatchResponse
