module Network.Livy.Env
  ( -- * Creating an environment
    Env (..)
  , HasEnv (..)
  , newEnv
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.ByteString as S
import           Network.HTTP.Client


-- | Environment required to make requests to Livy.
data Env = Env
  { _envManager :: !Manager
  , _envHost    :: !S.ByteString
  , _envPort    :: !Int
  }


class HasEnv a where
  environment :: Lens' a Env
  {-# MINIMAL environment #-}

  -- | 'Manager' used to create and manage HTTP connections.
  envManager :: Lens' a Manager

  -- | The host name.
  envHost :: Lens' a S.ByteString

  -- | The port number.
  envPort :: Lens' a Int

  envManager = environment . lens _envManager (\s a -> s {_envManager = a })
  envHost    = environment . lens _envHost (\s a -> s { _envHost = a })
  envPort    = environment . lens _envPort (\s a -> s { _envPort = a })


instance HasEnv Env where
  environment = id


-- | Creates a new environment with a new 'Manager' with default settings.
newEnv :: MonadIO m => S.ByteString -> Int -> m Env
newEnv h p = do
  man <- liftIO $ newManager defaultManagerSettings
  return $ Env man h p
