{-# LANGUAGE TemplateHaskell #-}

module Network.Livy.Client.Types.Batch where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (Text)
import Data.Typeable

data Batch = Batch
  { _bId      :: !Int
  , _bAppId   :: !Text
  , _bAppInfo :: !Object
  , _bLog     :: ![Text]
  , _bState   :: !Text
  } deriving (Eq, Show, Typeable)

makeLenses ''Batch

instance ToJSON Batch where
  toJSON b = object
    [ "id"      .= (b ^. bId)
    , "appId"   .= (b ^. bAppId)
    , "appInfo" .= (b ^. bAppInfo)
    , "log"     .= (b ^. bLog)
    , "state"   .= (b ^. bState)
    ]

instance FromJSON Batch where
  parseJSON = withObject "Batch" $ \o -> Batch
    <$> o .: "id"
    <*> o .: "appId"
    <*> o .: "appInfo"
    <*> o .: "log"
    <*> o .: "state"
