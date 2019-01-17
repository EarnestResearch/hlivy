{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.CreateSession where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Typeable
import GHC.Generics (Generic)

import Network.Livy.Client.Internal.JSON
import Network.Livy.Client.Types.Session
import Network.Livy.Request
import Network.Livy.Types


-- | The 'CreateSession' request object.
data CreateSession = CreateSession
  { _csKind                     :: SessionKind
  , _csProxyUser                :: Maybe Text
  , _csJars                     :: Maybe [Text]
  , _csPyFiles                  :: Maybe [Text]
  , _csFiles                    :: Maybe [Text]
  , _csDriverMemory             :: Maybe Text
  , _csDriverCores              :: Maybe Int
  , _csExecutorMemory           :: Maybe Text
  , _csExecutorCores            :: Maybe Int
  , _csNumExecutors             :: Maybe Int
  , _csArchives                 :: Maybe [Text]
  , _csQueue                    :: Maybe Text
  , _csName                     :: Maybe Text
  , _csConf                     :: Maybe Object
  , _csHeartbeatTimeoutInSecond :: Maybe Int
  } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CreateSession
deriveToJSON ((recordPrefixOptions 3) { omitNothingFields = True }) ''CreateSession

instance ToPath CreateSession where
  toPath = const "/sessions"

instance LivyRequest CreateSession where
  request = postJSON


-- | Creates a value of 'CreateSession' with the minimum fields required to make a request.
createSession :: SessionKind -> CreateSession
createSession k = CreateSession
  { _csKind = k
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
  {_csrSession :: Session
  } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CreateSessionResponse
deriveFromJSON (recordPrefixOptions 4) ''CreateSessionResponse
type instance LivyResponse CreateSession = CreateSessionResponse
