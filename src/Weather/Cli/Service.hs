module Weather.Cli.Service
  ( runCommand
  )
where

import           Relude

import           Weather.Cli.Effects.RemoteWeatherApi.Class ( RemoteWeatherApi )
import           Weather.Cli.Effects.ApiKey                 ( GetApiKey
                                                            , SetApiKey
                                                            )
import           Weather.Cli.Types

import qualified Weather.Cli.Effects.RemoteWeatherApi.Class as RemoteWeatherApi
import qualified Weather.Cli.Effects.ApiKey                 as ApiKey

runCommand 
  :: ( RemoteWeatherApi m
     , GetApiKey m
     , SetApiKey m
     ) 
  => Command 
  -> m (Either Text Text)
runCommand (SetApiKey         apiKey ) = setApiKey apiKey
runCommand (GetCurrentWeather request) = getCurrentWeather request


-- * Handle SetApiKey command
setApiKey :: SetApiKey m => Text -> m (Either Text Text)
setApiKey apiKey = ApiKey.setApiKey apiKey $> Right "API Key was set."


-- * Handle GetCurrentWeather command
getCurrentWeather :: (GetApiKey m, RemoteWeatherApi m) => WeatherRequest -> m (Either Text Text)
getCurrentWeather r@WeatherRequest {..} =
  RemoteWeatherApi.getCurrentWeather r
    <&> bimap show (formatWeather reqMeasureUnit)

formatWeather :: MeasurementUnit -> CurrentWeatherResponse -> Text
formatWeather unit CurrentWeatherResponse {..} =
  "Weather: " <> weather <> "\n"
    <> "Temperature: " <> temperature <> "\n"
    <> "Min temperature: " <> minTemperature <> "\n"
    <> "Max temperature: " <> maxTemperature <> "\n"
    <> "Feels like: " <> feelsLike <> "\n"
    <> "Pressure: " <> pressure <> "\n"
    <> "Humidity: " <> humidity
 where
  temperature     = show respTemperature <> " " <> temperatureUnit
  minTemperature  = show respMinTemperature <> " " <> temperatureUnit
  maxTemperature  = show respMaxTemperature <> " " <> temperatureUnit
  feelsLike       = show respFeelsLike <> " " <> temperatureUnit
  weather         = toText respMain <> " - " <> toText respDescription
  pressure        = show respPressure <> " hPa"
  humidity        = show respHumidity <> "%"
  temperatureUnit = case unit of
    Imperial -> "°F"
    Metric   -> "°C"
    Standard -> "K"
