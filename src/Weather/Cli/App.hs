module Weather.Cli.App
  ( MonadApp
  , Env
  , HasRemoteWeatherApiEnv(..)
  , makeEnv
  , runMonadApp
  )
where

import           Relude

import           Weather.Cli.Effects.RemoteWeatherApi.Env

newtype MonadApp a = MonadApp { fromMonadApp :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

newtype Env = Env 
  { remoteWeatherApiEnv :: RemoteWeatherApiEnv
  }

makeEnv :: MonadIO m => m Env
makeEnv = fmap Env makeRemoteWeatherApiEnv

runMonadApp :: Env -> MonadApp a -> IO a
runMonadApp env = usingReaderT env . fromMonadApp


class Monad m => HasRemoteWeatherApiEnv m where
  getRemoteWeatherApiEnv :: m RemoteWeatherApiEnv
instance HasRemoteWeatherApiEnv MonadApp where
  getRemoteWeatherApiEnv = asks remoteWeatherApiEnv
