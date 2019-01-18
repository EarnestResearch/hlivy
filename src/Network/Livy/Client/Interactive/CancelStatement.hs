{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.CancelStatement where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Text (Text)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'CancelStatement' request object.
data CancelStatement = CancelStatement
  { _cstmSessionId   :: Int -- ^ Id of the session.
  , _cstmStatementId :: Int -- ^ Id of the statement.
  } deriving (Eq, Show, Typeable)

makeLenses ''CancelStatement
deriveToJSON (recordPrefixOptions 5) ''CancelStatement

instance ToPath CancelStatement where
  toPath r = C.pack $ "/sessions/"
    <> show (r ^. cstmSessionId)
    <> "/statements/"
    <> show (r ^. cstmStatementId)
    <> "/cancel"

instance LivyRequest CancelStatement where
  request = postJSON


-- | Creates a value of 'CancelStatement' with the minimum fields required to make a request.
cancelStatement :: Int -> Int -> CancelStatement
cancelStatement = CancelStatement


-- | The 'CancelStatement' response body.
newtype CancelStatementResponse = CancelStatementResponse
  { _csrMsg :: Text -- ^ Is always "cancelled".
  } deriving (Eq, Show, Typeable)

makeLenses ''CancelStatementResponse
deriveFromJSON (recordPrefixOptions 4) ''CancelStatementResponse
type instance LivyResponse CancelStatement = CancelStatementResponse
