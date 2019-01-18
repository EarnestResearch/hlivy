{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Network.Livy.Client.Interactive.KillSession
  ( -- * The request.
    KillSession (..)
  , killSession
    -- ** Request lenses.
  , ksSessionId
    -- * The response.
  , KillSessionResponse (..)
    -- ** Response lenses.
  , ksrMsg
  ) where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import           Data.Text (Text)
import           Data.Typeable

import           Network.Livy.Client.Internal.JSON
import           Network.Livy.Request
import           Network.Livy.Types


-- | The 'KillSession' request object.
newtype KillSession = KillSession
  { _ksSessionId :: Int -- ^ Id of the session.
  } deriving (Eq, Show, Typeable)

makeLenses ''KillSession

instance ToPath KillSession where
  toPath s = C.pack $ "/sessions" <> show (s ^. ksSessionId)

instance LivyRequest KillSession where
  request = delete


-- | Creates a value of 'KillSession' with the minimum fields required to make a request.
killSession :: Int -> KillSession
killSession = KillSession


-- | The 'KillSession' response body.
newtype KillSessionResponse = KillSessionResponse
  { _ksrMsg :: Text -- ^ Message from the Livy server.
  } deriving (Eq, Show, Typeable)

makeLenses ''KillSessionResponse
deriveFromJSON (recordPrefixOptions 4) ''KillSessionResponse
type instance LivyResponse KillSession = KillSessionResponse
