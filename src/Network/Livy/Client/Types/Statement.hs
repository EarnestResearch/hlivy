{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|
Module      :  Network.Livy.Client.Types.Statement
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Client.Types.Statement
  ( -- * Statements for interactive sessions
    Statement (..)
  , StatementId (..)
  , StatementState (..)
  , StatementOutput (..)
  , StatementData
    -- ** Lenses
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
import qualified Data.HashMap.Strict as Map
#if ! MIN_VERSION_base(4,12,0)
import           Data.Monoid ((<>))
#endif
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Internal.Text


-- | The present state of a submitted 'Statement'.
data StatementState
  = StatementWaiting -- ^ Statement is enqueued but execution hasn't started.
  | StatementRunning -- ^ Statement is currently running.
  | StatementAvailable -- ^ Statement has a response ready.
  | StatementError -- ^ Statement failed.
  | StatementCancelling -- ^ Statement is being cancelled.
  | StatementCancelled -- ^ Statement is cancelled.
    deriving (Bounded, Enum, Eq, Show, Typeable)

instance ToText StatementState where
  toText = T.toLower . T.drop 9 . T.pack . show

instance ToJSON StatementState where
  toJSON = String . toText

instance FromJSON StatementState where
  parseJSON = withText "StatementState" $ \t ->
    case lookup t toTextLookup of
      Just st -> return st
      Nothing -> fail . T.unpack $ "Unknown statement state: " <> t


-- | Statement output.
type StatementData = Map.HashMap Text (Maybe Text)


-- | The output of a completed statement.
data StatementOutput = StatementOutput
  { _stoStatus         :: !(Maybe Text) -- ^ Execution status.
  , _stoExecutionCount :: !(Maybe Integer) -- ^ A monotonically increasing number.
  , _stoData           :: !(Maybe StatementData) -- ^ Statement output.
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
    <$> o .:? "status"
    <*> o .:? "execution_count"
    <*> o .:? "data"


-- | The id of this statement.
newtype StatementId = StatementId Int
  deriving (Eq, Show, Typeable, ToText, ToJSON, FromJSON)


-- | A 'Statement' represents the result of an execution statement.
data Statement = Statement
  { _stId     :: !StatementId -- ^ The statement id.
  , _stCode   :: !(Maybe Text) -- ^ The execution code.
  , _stState  :: !(Maybe StatementState) -- ^ The execution state.
  , _stOutput :: !(Maybe StatementOutput) -- ^ The execution output.
  } deriving (Eq, Show, Typeable)

makeLenses ''Statement
deriveJSON (recordPrefixOptions 3) ''Statement
