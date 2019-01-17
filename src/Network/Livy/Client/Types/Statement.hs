{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Livy.Client.Types.Statement where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Typeable


data StatementState
  = StatementWaiting
  | StatementRunning
  | StatementAvailable
  | StatementError
  | StatementCancelling
  | StatementCancelled
    deriving (Eq, Typeable)

instance Show StatementState where
  show StatementWaiting    = "waiting"
  show StatementRunning    = "running"
  show StatementAvailable  = "available"
  show StatementError      = "error"
  show StatementCancelling = "cancelling"
  show StatementCancelled  = "cancelled"

instance ToJSON StatementState where
  toJSON = String . pack . show

instance FromJSON StatementState where
  parseJSON = withText "StatementState" $ \case
    "waiting"    -> return StatementWaiting
    "running"    -> return StatementRunning
    "available"  -> return StatementAvailable
    "error"      -> return StatementError
    "cancelling" -> return StatementCancelling
    "cancelled"  -> return StatementCancelled
    s            -> fail . unpack $ "Unknown statement state: " <> s


data StatementOutput = StatementOutput
  { _stoStatus         :: !Text
  , _stoExecutionCount :: !Integer
  , _stoData           :: !Object
  } deriving (Eq, Show, Typeable)

makeLenses ''StatementOutput

instance ToJSON StatementOutput where
  toJSON (StatementOutput s c d) = object
    [ "status"          .= s
    , "execution_count" .= c
    , "data"            .= d
    ]

instance FromJSON StatementOutput where
  parseJSON = withObject "StatementOutput" $ \o -> StatementOutput
    <$> o .: "status"
    <*> o .: "execution_count"
    <*> o .: "data"


data Statement = Statement
  { _stId     :: !Int
  , _stCode   :: !Text
  , _stState  :: !StatementState
  , _stOutput :: !StatementOutput
  } deriving (Eq, Show, Typeable)

makeLenses ''Statement

instance ToJSON Statement where
  toJSON (Statement i c s o) = object
    [ "id"     .= i
    , "code"   .= c
    , "state"  .= s
    , "output" .= o
    ]
