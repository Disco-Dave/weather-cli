-- | An effect for interacting with a remote weather API.
module Weather.Cli.Effects.RemoteWeatherApi where

import           Relude

import           Weather.Cli.Types
import           Weather.Cli.App

import           Control.Lens
import           GHC.Show                       ( show )
import           Network.Wreq


-- | A error that can occur while interacting with the
-- remote weather API.
data RemoteWeatherApiError
  = ZipCodeNotFound
  | UnableToReachServer
  | MalformedResponse

instance Show RemoteWeatherApiError where
  show ZipCodeNotFound = "Unable to find the weather for requested zip code."
  show UnableToReachServer = "Unable to reach to the remote server."
  show MalformedResponse = "Response from server was not in the expected format."

-- | An effect for interacting with a remote weather API.
class Monad m => RemoteWeatherApi m where
  -- | Get the weather report.
  getWeather :: WeatherRequest -> m (Either RemoteWeatherApiError WeatherResponse)



instance RemoteWeatherApi MonadApp where
  getWeather WeatherRequest {..} = do
    apiKey <- asks apiKey

    let units = case reqMeasureUnit of
          Standard -> "standard"
          Imperial -> "imperial"
          Metric   -> "metric"

    let opts =
          defaults 
            & param "zip" .~ [fromUsZipCode reqUsZipCode <> ",us"]
            & param "appid" .~ [apiKey]
            & param "units" .~ [units]

    let openWeatherApiUrl = "https://api.openweathermap.org/data/2.5/weather"

    response <- liftIO $ getWith opts openWeatherApiUrl

    undefined
