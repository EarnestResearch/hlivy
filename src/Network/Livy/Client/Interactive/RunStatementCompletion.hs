{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.RunStatementCompletion
  ( -- * Request.
    RunStatementCompletion (..)
  , runStatementCompletion
    -- ** Request lenses.
  , rscSessionId
  , rscCode
  , rscKind
  , rscCursor
    -- * Response.
  , RunStatementCompletionResponse
  , rscrCandidates
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Text (Text)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Session
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'RunStatementCompletion' request object.
data RunStatementCompletion = RunStatementCompletion
  { _rscSessionId :: Int -- ^ Id of the session.
  , _rscCode      :: Maybe Text -- ^ The code for which completion proposals are requested.
  , _rscKind      :: Maybe SessionKind -- ^ The kind of code to execute.
  , _rscCursor    :: Maybe Text -- ^ Cursor position to get proposals.
  } deriving (Eq, Show, Typeable)

makeLenses ''RunStatementCompletion
deriveToJSON (recordPrefixOptions 4) ''RunStatementCompletion

instance ToPath RunStatementCompletion where
  toPath r = C.pack $ "/sessions/" <> show (r ^. rscSessionId) <> "/completion"

instance LivyRequest RunStatementCompletion where
  request = postJSON


-- | Creates a value of 'CancelStatement' with the minimum fields required to make a request.
runStatementCompletion :: Int -> RunStatementCompletion
runStatementCompletion n = RunStatementCompletion n Nothing Nothing Nothing


-- | The 'RunStatementCompletion' response body.
newtype RunStatementCompletionResponse = RunStatementCompletionResponse
  { _rscrCandidates :: [Text] -- ^ Code completion proposals.
  } deriving (Eq, Show, Typeable)

makeLenses ''RunStatementCompletionResponse
deriveFromJSON (recordPrefixOptions 5) ''RunStatementCompletionResponse
type instance LivyResponse RunStatementCompletion = RunStatementCompletionResponse
