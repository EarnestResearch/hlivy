{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.RunStatementCompletion
  ( -- * The request
    RunStatementCompletion (..)
  , runStatementCompletion
    -- ** Request lenses
  , rscSessionId
  , rscCode
  , rscKind
  , rscCursor
    -- * The response
  , RunStatementCompletionResponse (..)
    -- ** Response lenses
  , rscrCandidates
  ) where

import Control.Lens
import Data.Aeson.TH
import Data.Text (Text)
import Data.Typeable

import Network.Livy.Client.Internal.JSON
import Network.Livy.Client.Types.Session
import Network.Livy.Internal.Text
import Network.Livy.Request
import Network.Livy.Types


-- | The 'RunStatementCompletion' request object.
data RunStatementCompletion = RunStatementCompletion
  { _rscSessionId :: SessionId -- ^ Id of the session.
  , _rscCode      :: Maybe Text -- ^ The code for which completion proposals are requested.
  , _rscKind      :: Maybe SessionKind -- ^ The kind of code to execute.
  , _rscCursor    :: Maybe Text -- ^ Cursor position to get proposals.
  } deriving (Eq, Show, Typeable)

makeLenses ''RunStatementCompletion

instance ToPath RunStatementCompletion where
  toPath r = toPath ["sessions", toText $ r ^. rscSessionId, "completion"]

instance LivyRequest RunStatementCompletion where
  request r = postBody r
    [ ("code", toText $ r ^. rscCode)
    , ("kind", toText $ r ^. rscKind)
    , ("cursor", toText $ r ^. rscCursor)
    ]


-- | Creates a value of 'CancelStatement' with the minimum fields required to make a request.
runStatementCompletion :: SessionId -> RunStatementCompletion
runStatementCompletion sid = RunStatementCompletion sid Nothing Nothing Nothing


-- | The 'RunStatementCompletion' response body.
newtype RunStatementCompletionResponse = RunStatementCompletionResponse
  { _rscrCandidates :: [Text] -- ^ Code completion proposals.
  } deriving (Eq, Show, Typeable)

makeLenses ''RunStatementCompletionResponse
deriveFromJSON (recordPrefixOptions 5) ''RunStatementCompletionResponse
type instance LivyResponse RunStatementCompletion = RunStatementCompletionResponse
