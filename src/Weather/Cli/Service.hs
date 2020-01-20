module Weather.Cli.Service where

import           Relude

import           Weather.Cli.Effects.ReadFromConsole        ( ReadFromConsole )
import           Weather.Cli.Effects.RemoteWeatherApi.Class ( RemoteWeatherApi )
import           Weather.Cli.Effects.WriteToConsole         ( WriteToConsole )
import           Weather.Cli.Types

import qualified Weather.Cli.Effects.ReadFromConsole        as ReadFromConsole
import qualified Weather.Cli.Effects.RemoteWeatherApi.Class as RemoteWeatherApi
import qualified Weather.Cli.Effects.WriteToConsole         as WriteToConsole


reportWeather :: (WriteToConsole m, RemoteWeatherApi m, ReadFromConsole m) => m ()
reportWeather = do
  req@WeatherRequest {..} <- ReadFromConsole.getRequest
  RemoteWeatherApi.getWeather req >>= either
    (WriteToConsole.writeLineToStdErr . show)
    (WriteToConsole.writeLineToStdOut . formatWeather reqMeasureUnit)


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
