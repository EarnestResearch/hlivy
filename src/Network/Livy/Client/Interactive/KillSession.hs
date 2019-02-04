{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module      :  Network.Livy.Client.Interactive.KillSession
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Client.Interactive.KillSession
  ( -- * The request
    KillSession (..)
  , killSession
    -- ** Request lenses
  , ksSessionId
    -- * The response
  , KillSessionResponse (..)
    -- ** Response lenses
  , ksrMsg
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


-- | The 'KillSession' request object.
newtype KillSession = KillSession
  { _ksSessionId :: SessionId -- ^ Id of the session.
  } deriving (Eq, Show, Typeable)

makeLenses ''KillSession

instance ToPath KillSession where
  toPath s = toPath ["sessions", toText $ s ^. ksSessionId]

instance LivyRequest KillSession where
  request = delete


-- | Creates a value of 'KillSession' with the minimum fields required to make a request.
killSession :: SessionId -> KillSession
killSession = KillSession


-- | The 'KillSession' response body.
newtype KillSessionResponse = KillSessionResponse
  { _ksrMsg :: Text -- ^ Is always "deleted".
  } deriving (Eq, Show, Typeable)

makeLenses ''KillSessionResponse
deriveFromJSON (recordPrefixOptions 4) ''KillSessionResponse
type instance LivyResponse KillSession = KillSessionResponse
