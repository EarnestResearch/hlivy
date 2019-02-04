{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module      :  Network.Livy.Client.Interactive.RunStatement
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Client.Interactive.RunStatement
  ( -- * The request
    RunStatement (..)
  , runStatement
    -- ** Request lenses
  , rsSessionId
  , rsCode
  , rsKind
    -- * The response
  , RunStatementResponse (..)
    -- ** Response lenses
  , rsrStatement
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Typeable

import Network.Livy.Client.Types.Session
import Network.Livy.Client.Types.Statement
import Network.Livy.Internal.Text
import Network.Livy.Request
import Network.Livy.Types


-- | The 'RunStatement' request object.
data RunStatement = RunStatement
  { _rsSessionId :: SessionId -- ^ Id of the session.
  , _rsCode      :: Text -- ^ The code to execute.
  , _rsKind      :: SessionKind -- ^ The kind of code to execute.
  } deriving (Eq, Show, Typeable)

makeLenses ''RunStatement

instance ToPath RunStatement where
  toPath r = toPath ["sessions", toText $ r ^. rsSessionId, "statements"]

instance LivyRequest RunStatement where
  request r = postBody r [("code", r ^. rsCode), ("kind", toText $ r ^. rsKind)]


-- | Creates a value of 'RunStatement' with the minimum fields required to make a request.
runStatement :: SessionId -> Text -> SessionKind -> RunStatement
runStatement = RunStatement


-- | The 'RunStatement' response body.
newtype RunStatementResponse = RunStatementResponse
  { _rsrStatement :: Statement -- ^ The 'Statement' object.
  } deriving (Eq, Show, Typeable)

makeLenses ''RunStatementResponse
deriveFromJSON (defaultOptions { unwrapUnaryRecords = True }) ''RunStatementResponse
type instance LivyResponse RunStatement = RunStatementResponse
