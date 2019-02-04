{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      :  Network.Livy.Monad
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Monad
  ( -- * Running Livy actions
    Livy
  , LivyT (..)
  , LivyConstraint
  , runLivy
  , runLivyT
  ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson

import Network.Livy.Env
import Network.Livy.Types


-- | A specialization of 'LivyT'.
type Livy = LivyT Env (ResourceT IO)


-- | Run the 'Livy' monad.
runLivy :: HasEnv r => r -> Livy a -> IO a
runLivy e = runResourceT . runLivyT (e ^. environment)


-- | 'LivyT' transformer.
newtype LivyT r m a = LivyT { unLivyT :: ReaderT r m a }
  deriving ( Applicative, Functor, Monad
           , MonadTrans, MonadIO, MonadReader r
           , MonadThrow, MonadCatch, MonadResource
           )


-- | Run a LivyT action with the given environment.
runLivyT :: HasEnv r => r -> LivyT r m a -> m a
runLivyT r (LivyT m) = runReaderT m r


-- | An alias for the constraints required to send a request to Livy.
type LivyConstraint r m a =
  ( HasEnv r, MonadIO m, MonadThrow m, MonadCatch m
  , MonadReader r m, LivyRequest a, FromJSON (LivyResponse a)
  )
