module Weather.Cli.Effects.RemoteWeatherApi.Env where

import           Relude

import           Network.HTTP.Client
import           Servant.Client


data RemoteWeatherApiEnv = RemoteWeatherApiEnv
  { remoteApiKey :: Text
  , remoteClientEnv :: ClientEnv
  }

makeRemoteWeatherApiEnv :: MonadIO m => Text -> m RemoteWeatherApiEnv
makeRemoteWeatherApiEnv apiKey = do
  manager' <- liftIO $ newManager defaultManagerSettings
  let baseUrl'   = BaseUrl Http "api.openweathermap.org" 80 "data/2.5"
      clientEnv' = mkClientEnv manager' baseUrl'
  pure $ RemoteWeatherApiEnv apiKey clientEnv'

