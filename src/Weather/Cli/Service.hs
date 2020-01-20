module Weather.Cli.Service where

import           Relude

import           Weather.Cli.Effects.Console    ( Console )
import           Weather.Cli.Effects.RemoteWeatherApi.Class
                                                ( RemoteWeatherApi )
import           Weather.Cli.Types

import qualified Weather.Cli.Effects.Console   as Console
import qualified Weather.Cli.Effects.RemoteWeatherApi.Class
                                               as RemoteWeatherApi


reportWeather :: (Console m, RemoteWeatherApi m) => WeatherRequest -> m ()
reportWeather request@WeatherRequest {..} =
  RemoteWeatherApi.getWeather request >>= either
    (Console.writeLineToStdErr . show)
    (Console.writeLineToStdOut . formatWeather reqMeasureUnit)


formatWeather :: MeasurementUnit -> WeatherResponse -> Text
formatWeather unit WeatherResponse {..} =
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
