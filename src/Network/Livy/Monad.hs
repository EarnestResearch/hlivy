{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Livy.Monad where

import Control.Monad.Reader
import Data.Aeson

import Network.Livy.Response
import Network.Livy.Types


newtype LivyT' r m a = LivyT' { unLivyT :: ReaderT r m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader r)


-- | 'LivyT' with a default environment.
type LivyT m a = LivyT' Env m a


-- | Run a Livy action with the given environment.
runLivyT :: HasEnv r => r -> LivyT' r m a -> m a
runLivyT r (LivyT' m) = runReaderT m r


-- | An alias for the constraints required to send a request to Livy.
type LivyConstraint r m a =
  (HasEnv r, MonadIO m, MonadReader r m, LivyRequest a, FromJSON (LivyResponse a))


-- | Send a request, returning the associated response if successful.
send :: LivyConstraint r m a => a -> m (Either LivyError (LivyResponse a))
send req = reader hasEnv >>= liftIO . parseHttp req
