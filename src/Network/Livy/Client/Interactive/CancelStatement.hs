{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.CancelStatement
  ( -- * The request
    CancelStatement (..)
  , cancelStatement
    -- ** Request lensese
  , cstmSessionId
  , cstmStatementId
    -- * The response
  , CancelStatementResponse (..)
    -- ** Response lenses
  , csrMsg
  ) where

import Control.Lens
import Data.Aeson.TH
import Data.Text (Text)
import Data.Typeable

import Network.Livy.Client.Internal.JSON
import Network.Livy.Client.Types.Session
import Network.Livy.Client.Types.Statement
import Network.Livy.Internal.Text
import Network.Livy.Request
import Network.Livy.Types


-- | The 'CancelStatement' request object.
data CancelStatement = CancelStatement
  { _cstmSessionId   :: SessionId -- ^ Id of the session.
  , _cstmStatementId :: StatementId -- ^ Id of the statement.
  } deriving (Eq, Show, Typeable)

makeLenses ''CancelStatement
deriveToJSON (recordPrefixOptions 5) ''CancelStatement

instance ToPath CancelStatement where
  toPath r = toPath
    [ "sessions", toText $ r ^. cstmSessionId
    , "statements", toText $ r ^. cstmStatementId
    , "cancel"
    ]

instance LivyRequest CancelStatement where
  request = post


-- | Creates a value of 'CancelStatement' with the minimum fields required to make a request.
cancelStatement :: SessionId -> StatementId -> CancelStatement
cancelStatement = CancelStatement


-- | The 'CancelStatement' response body.
newtype CancelStatementResponse = CancelStatementResponse
  { _csrMsg :: Text -- ^ Is always "cancelled".
  } deriving (Eq, Show, Typeable)

makeLenses ''CancelStatementResponse
deriveFromJSON (recordPrefixOptions 4) ''CancelStatementResponse
type instance LivyResponse CancelStatement = CancelStatementResponse
