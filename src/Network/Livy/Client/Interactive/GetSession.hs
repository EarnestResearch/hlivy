{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module      :  Network.Livy.Client.Interactive.GetSession
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Client.Interactive.GetSession
  ( -- * The request
    GetSession (..)
  , getSession
    -- ** Request lenses
  , gsSessionId
    -- * The response
  , GetSessionResponse (..)
    -- ** Response lenses
  , gsrSession
  ) where

import Control.Lens
import Data.Aeson.TH
import Data.Typeable

import Network.Livy.Client.Internal.JSON
import Network.Livy.Client.Types.Session
import Network.Livy.Internal.Text
import Network.Livy.Request
import Network.Livy.Types


-- | The 'GetSession' request object.
newtype GetSession = GetSession
  { _gsSessionId :: SessionId -- ^ Id of the session.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSession

instance ToPath GetSession where
  toPath r = toPath ["sessions", toText $ r ^. gsSessionId]

instance LivyRequest GetSession where
  request = get


-- | Creates a value of 'GetSession' with the minimum fields required to make a request.
getSession :: SessionId -> GetSession
getSession = GetSession


-- | The 'GetSession' response body.
newtype GetSessionResponse = GetSessionResponse
  { _gsrSession :: Session -- ^ The 'Session' object.
  } deriving (Eq, Show, Typeable)

makeLenses ''GetSessionResponse
deriveFromJSON ((recordPrefixOptions 3) { unwrapUnaryRecords = True }) ''GetSessionResponse
type instance LivyResponse GetSession = GetSessionResponse
