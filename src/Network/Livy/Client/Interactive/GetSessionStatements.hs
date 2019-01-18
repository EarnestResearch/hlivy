{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.GetSessionStatements
  ( -- * The request.
    GetSessionStatements (..)
  , getSessionStatements
    -- ** Request lenses.
  , gsstmsSessionId
    -- * The response.
  , GetSessionStatementsResponse (..)
    -- ** Response lenses.
  , gsstmsrStatements
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Client.Types.Statement
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'GetSessionStatements' request object.
newtype GetSessionStatements = GetSessionStatements
  { _gsstmsSessionId :: Int -- ^ Id of the session.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionStatements

instance ToPath GetSessionStatements where
  toPath r = C.pack $ "/sessions/" <> show (r ^. gsstmsSessionId) <> "/statements"

instance LivyRequest GetSessionStatements where
  request = get


-- | Creates a value of 'GetSessionStatements' with the minimum fields required to make a request.
getSessionStatements :: Int -> GetSessionStatements
getSessionStatements = GetSessionStatements


-- | The 'GetSessionStatements' response body.
newtype GetSessionStatementsResponse = GetSessionStatementsResponse
  { _gsstmsrStatements :: [Statement] -- ^ Statement list.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionStatementsResponse
deriveFromJSON (recordPrefixOptions 8) ''GetSessionStatementsResponse
type instance LivyResponse GetSessionStatements = GetSessionStatementsResponse
