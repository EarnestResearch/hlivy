{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.RunStatement
  ( -- * The request.
    RunStatement (..)
  , runStatement
    -- ** Request lenses.
  , rsSessionId
  , rsCode
  , rsKind
    -- * The response.
  , RunStatementResponse (..)
    -- ** Response lenses.
  , rsrStatement
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Text (Text)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Session
import           Network.Livy.Client.Types.Statement
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'RunStatement' request object.
data RunStatement = RunStatement
  { _rsSessionId :: Int -- ^ Id of the session.
  , _rsCode      :: Text -- ^ The code to execute.
  , _rsKind      :: Maybe SessionKind -- ^ The kind of code to execute.
  } deriving (Eq, Show, Typeable)

makeLenses ''RunStatement
deriveToJSON (recordPrefixOptions 3) ''RunStatement

instance ToPath RunStatement where
  toPath r = C.pack $ "/sessions/" <> show (r ^. rsSessionId) <> "/statements"

instance LivyRequest RunStatement where
  request = postJSON


-- | Creates a value of 'GetSessions' with the minimum fields required to make a request.
runStatement :: Int -> Text -> RunStatement
runStatement n c = RunStatement n c Nothing


-- | The 'RunStatement' response body.
newtype RunStatementResponse = RunStatementResponse
  { _rsrStatement :: Statement
  } deriving (Eq, Show, Typeable)

makeLenses ''RunStatementResponse
deriveFromJSON (recordPrefixOptions 4) ''RunStatementResponse
type instance LivyResponse RunStatement = RunStatementResponse
