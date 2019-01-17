{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Livy.Client.Types.Statement
  ( -- * Statements for interactive sessions.
    Statement (..)
  , StatementState (..)
  , StatementOutput (..)
    -- ** Lenses.
  , stoStatus
  , stoExecutionCount
  , stoData
  , stId
  , stCode
  , stState
  , stOutput
  ) where

import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON


-- | The present state of a submitted 'Statement'.
data StatementState
  = StatementWaiting -- ^ Statement is enqueued but execution hasn't started.
  | StatementRunning -- ^ Statement is currently running.
  | StatementAvailable -- ^ Statement has a response ready.
  | StatementError -- ^ Statement failed.
  | StatementCancelling -- ^ Statement is being cancelled.
  | StatementCancelled -- ^ Statement is cancelled.
    deriving (Eq, Show, Typeable)

instance ToJSON StatementState where
  toJSON = String . T.toLower . T.drop 9 .T.pack . show

instance FromJSON StatementState where
  parseJSON = withText "StatementState" $ \case
    "waiting"    -> return StatementWaiting
    "running"    -> return StatementRunning
    "available"  -> return StatementAvailable
    "error"      -> return StatementError
    "cancelling" -> return StatementCancelling
    "cancelled"  -> return StatementCancelled
    s            -> fail . T.unpack $ "Unknown statement state: " <> s


-- | The output of a completed statement.
data StatementOutput = StatementOutput
  { _stoStatus         :: !Text -- ^ Execution status.
  , _stoExecutionCount :: !Integer -- ^ A monotonically increasing number.
  , _stoData           :: !Object -- ^ Statement output.
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


-- ^ A 'Statement' represents the result of an execution statement.
data Statement = Statement
  { _stId     :: !Int -- ^ The statement id.
  , _stCode   :: !Text -- ^ The execution code.
  , _stState  :: !StatementState -- ^ The execution state.
  , _stOutput :: !StatementOutput -- ^ The execution output.
  } deriving (Eq, Show, Typeable)

makeLenses ''Statement
deriveJSON (recordPrefixOptions 3) ''Statement
