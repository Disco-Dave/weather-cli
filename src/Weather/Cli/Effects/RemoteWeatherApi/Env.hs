module Weather.Cli.Effects.RemoteWeatherApi.Env where

import           Relude

import           Network.HTTP.Client
import           Servant.Client


newtype RemoteWeatherApiEnv = RemoteWeatherApiEnv
  { remoteClientEnv :: ClientEnv
  }

makeRemoteWeatherApiEnv :: MonadIO m => m RemoteWeatherApiEnv
makeRemoteWeatherApiEnv = do
  manager' <- liftIO $ newManager defaultManagerSettings
  let baseUrl'   = BaseUrl Http "api.openweathermap.org" 80 "data/2.5"
      clientEnv' = mkClientEnv manager' baseUrl'
  pure $ RemoteWeatherApiEnv clientEnv'
