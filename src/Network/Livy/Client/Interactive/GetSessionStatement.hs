{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.GetSessionStatement where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Statement
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'GetSessionStatement' request object.
data GetSessionStatement = GetSessionStatement
  { _gsstmSessionId   :: Int -- ^ Id of the session.
  , _gsstmStatementId :: Int -- ^ Id of the statement.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionStatement

instance ToPath GetSessionStatement where
  toPath r = C.pack $ "/sessions/"
    <> show (r ^. gsstmSessionId)
    <> "/statements/"
    <> show (r ^. gsstmStatementId)

instance LivyRequest GetSessionStatement where
  request = get


-- | Creates a value of 'GetSessionStatement' with the minimum fields required to make a request.
getSessionStatement :: Int -> Int -> GetSessionStatement
getSessionStatement = GetSessionStatement


-- | The 'GetSessionStatement' response body.
newtype GetSessionStatementResponse = GetSessionStatementResponse
  { _gsstmrMessage :: Statement -- ^ The 'Statement' object.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionStatementResponse
deriveFromJSON (recordPrefixOptions 7) ''GetSessionStatementResponse
type instance LivyResponse GetSessionStatement = GetSessionStatementResponse
