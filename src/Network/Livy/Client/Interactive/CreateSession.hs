{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module      :  Network.Livy.Client.Interactive.CreateSession
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Client.Interactive.CreateSession
  ( -- * The request
    CreateSession (..)
  , createSession
   -- ** Request lenses
  , csKind
  , csProxyUser
  , csJars
  , csPyFiles
  , csFiles
  , csDriverMemory
  , csDriverCores
  , csExecutorMemory
  , csExecutorCores
  , csNumExecutors
  , csArchives
  , csQueue
  , csName
  , csConf
  , csHeartbeatTimeoutInSecond
    -- * The response
  , CreateSessionResponse (..)
    -- ** Response lenses
  , csrSession
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Session
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'CreateSession' request object.
data CreateSession = CreateSession
  { _csKind                     :: Maybe SessionKind
  -- ^ The session kind. Note that, starting with Livy-0.5.0, this parameter is not required.
  -- Instead, users should specify the session kind in statement creation.
  , _csProxyUser                :: Maybe Text -- ^ User to impersonate when starting the session.
  , _csJars                     :: Maybe [Text] -- ^ JARs to be used in this session.
  , _csPyFiles                  :: Maybe [Text] -- ^ Python files to be used in this session.
  , _csFiles                    :: Maybe [Text] -- ^ Files to be used in this session.
  , _csDriverMemory             :: Maybe Text -- ^ Amount of memory to use for the driver process.
  , _csDriverCores              :: Maybe Int -- ^ Number of cores to use for the driver process.
  , _csExecutorMemory           :: Maybe Text -- ^ Amount of memory to use per executor process.
  , _csExecutorCores            :: Maybe Int -- ^ Number of cores to use for each executor.
  , _csNumExecutors             :: Maybe Int -- ^ Number of executors to launch for this session.
  , _csArchives                 :: Maybe [Text] -- ^ Archives to be used in this session.
  , _csQueue                    :: Maybe Text  -- ^ The name of the YARN queue submitted to.
  , _csName                     :: Maybe Text -- ^ The name of this session.
  , _csConf                     :: Maybe (Map.HashMap Text Text) -- ^ Spark configuration properties.
  , _csHeartbeatTimeoutInSecond :: Maybe Int -- ^ Timout in seconds after which to orphan the session.
  } deriving (Eq, Show, Typeable)

makeLenses ''CreateSession
deriveToJSON ((recordPrefixOptions 3) { omitNothingFields = True }) ''CreateSession

instance ToPath CreateSession where
  toPath = const "sessions"

instance LivyRequest CreateSession where
  request = postJSON


-- | Creates a value of 'CreateSession' with the minimum fields required to make a request.
createSession :: CreateSession
createSession = CreateSession
  { _csKind = Nothing
  , _csProxyUser = Nothing
  , _csJars = Nothing
  , _csPyFiles = Nothing
  , _csFiles = Nothing
  , _csDriverMemory = Nothing
  , _csDriverCores = Nothing
  , _csExecutorMemory = Nothing
  , _csExecutorCores = Nothing
  , _csNumExecutors = Nothing
  , _csArchives = Nothing
  , _csQueue = Nothing
  , _csName = Nothing
  , _csConf = Nothing
  , _csHeartbeatTimeoutInSecond = Nothing
  }


-- | The 'CreateSession' response body.
newtype CreateSessionResponse = CreateSessionResponse
  { _csrSession :: Session -- ^ The created 'Session'.
  } deriving (Eq, Show, Typeable)

makeLenses ''CreateSessionResponse
deriveFromJSON (defaultOptions { unwrapUnaryRecords = True }) ''CreateSessionResponse
type instance LivyResponse CreateSession = CreateSessionResponse
