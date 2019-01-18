{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.GetSessionStatement
  ( -- * The request
    GetSessionStatement (..)
  , getSessionStatement
    -- ** Request lenses
  , gsstmSessionId
  , gsstmStatementId
    -- * The response
  , GetSessionStatementResponse (..)
    -- ** Response lenses
  , gsstmrStatement
  ) where

import Control.Lens
import Data.Aeson.TH
import Data.Typeable

import Network.Livy.Client.Types.Session
import Network.Livy.Client.Types.Statement
import Network.Livy.Internal.Text
import Network.Livy.Request
import Network.Livy.Types


-- | The 'GetSessionStatement' request object.
data GetSessionStatement = GetSessionStatement
  { _gsstmSessionId   :: SessionId -- ^ Id of the session.
  , _gsstmStatementId :: StatementId -- ^ Id of the statement.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionStatement

instance ToPath GetSessionStatement where
  toPath r = toPath
    [ "sessions", toText $ r ^. gsstmSessionId
    , "statements", toText $ r ^. gsstmStatementId
    ]

instance LivyRequest GetSessionStatement where
  request = get


-- | Creates a value of 'GetSessionStatement' with the minimum fields required to make a request.
getSessionStatement :: SessionId -> StatementId -> GetSessionStatement
getSessionStatement = GetSessionStatement


-- | The 'GetSessionStatement' response body.
newtype GetSessionStatementResponse = GetSessionStatementResponse
  { _gsstmrStatement :: Statement -- ^ The 'Statement' object.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionStatementResponse
deriveFromJSON (defaultOptions { unwrapUnaryRecords = True }) ''GetSessionStatementResponse
type instance LivyResponse GetSessionStatement = GetSessionStatementResponse
