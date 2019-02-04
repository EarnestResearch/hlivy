{-|
Module      :  Network.Livy
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable

@hlivy@ is a Haskell library that provides bindings to the Apache Livy REST API,
which enables one to easily launch Spark applications -- either in an interactive
or batch fashion -- via HTTP requests to the Livy server running on the master node
of a Spark cluster.

Usage:

In general, to use this library, one creates a request by using a default constructor
and modifying it, as needed, with the provided request lenses.  Wrapping this request
with 'send' creates a Livy action that can be ran with 'runLivy'.

For example, to run a Spark application whose main class is @com.company.my_app@ and
to request 4 cores per executor from the cluster:

@
import Network.Livy
import Control.Lens ((?~), (&))

main :: IO (Either 'LivyError' 'CreateBatchResponse')
main = do
  env <- 'newEnv' "localhost" 8998
  let req = 'createBatch' "\/user\/hadoop\/my-app.jar"
          & 'cbClassName' ?~ "com.company.my_app"
          & 'cbExecutorCores' ?~ 4
  'runLivy' env $ 'send' req
@

For more example usage, see [the examples](https:\/\/github.com\/EarnestResearch\/hlivy\/tree\/1.0.0\/examples).
-}

module Network.Livy
  ( -- * Running Livy actions
    Livy
  , LivyT (..)
  , runLivy
  , runLivyT
  , send

    -- * Environment
  , Env (..)
  , HasEnv (..)
  , newEnv

    -- * Request/response
  , LivyRequest (..)
  , LivyResponse

    -- * Livy actions
  , module Network.Livy.Client.Batch
  , module Network.Livy.Client.Interactive

    -- * REST objects
    -- ** Batch session
  , Batch (..)
  , BatchId (..)
  , BatchState (..)
  , BatchAppInfo
    -- *** Lenses
  , bId, bAppId, bAppInfo, bLog, bState
    -- ** Interactive session
  , Session (..)
  , SessionId (..)
  , SessionKind (..)
  , SessionState (..)
  , SessionAppInfo
    -- *** Lenses
  , sId, sAppId, sOwner, sProxyUser, sKind, sLog, sState, sAppInfo
    -- ** Statements for interactive sessions
  , Statement (..)
  , StatementId (..)
  , StatementState (..)
  , StatementOutput (..)
  , StatementData
    -- *** Lenses
  , stoStatus, stoExecutionCount, stoData, stId, stCode, stState, stOutput

    -- * Exceptions
  , LivyError (..)
  , LivyErrorType (..)
  , LivyHTTPErrorCode (..)

    -- * Misc.
  , APIVersion (..)
  ) where


import Network.Livy.Env
import Network.Livy.Monad
import Network.Livy.Response
import Network.Livy.Types

import Network.Livy.Client.Types.Batch
import Network.Livy.Client.Types.Session
import Network.Livy.Client.Types.Statement

import Network.Livy.Client.Batch
import Network.Livy.Client.Interactive


data APIVersion = V050Incubating -- ^ The version of Livy for this package release.

instance Show APIVersion where
  show V050Incubating = "0.5.0-incubating"
