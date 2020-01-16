-- | An effect for interacting with a remote weather API.
module Weather.Cli.Effects.RemoteWeatherApi where

import           Relude

import           Weather.Cli.Types

import           GHC.Show                       ( show )


-- | A error that can occur while interacting with the
-- remote weather API.
data RemoteWeatherApiError
  = ZipCodeNotFound
  | UnableToReachServer

instance Show RemoteWeatherApiError where
  show ZipCodeNotFound = "Unable to find the weather for requested zip code."
  show UnableToReachServer = "Unable to reach to the remote server."

-- | An effect for interacting with a remote weather API.
class Monad m => RemoteWeatherApi m where
  -- | Get the weather report.
  getWeather :: WeatherRequest -> m (Either RemoteWeatherApiError WeatherResponse)
