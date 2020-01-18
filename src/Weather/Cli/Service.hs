module Weather.Cli.Service where

import           Relude

import           Weather.Cli.Effects.Console
import           Weather.Cli.Effects.RemoteWeatherApi
import           Weather.Cli.Types


formatWeather :: MeasurementUnit -> WeatherResponse -> Text
formatWeather unit WeatherResponse {..} =
  let temperatureUnit = case unit of
        Imperial -> "°F"
        Metric   -> "°C"
        Standard -> "K"
      temperature    = show respTemperature <> " " <> temperatureUnit
      minTemperature = show respMinTemperature <> " " <> temperatureUnit
      maxTemperature = show respMaxTemperature <> " " <> temperatureUnit
      feelsLike      = show respFeelsLike <> " " <> temperatureUnit
      weather        = show respMain <> ". " <> show respDescription
      pressure       = show respPressure <> " hPa"
      humidity       = show respHumidity <> "%"
  in  "Weather: " <> weather <> "\n"
        <> "Temperature: " <> temperature <> "\n"
        <> "Min temperature: " <> minTemperature <> "\n"
        <> "Max temperature: " <> maxTemperature <> "\n"
        <> "Feels like: " <> feelsLike <> "\n"
        <> "Pressure: " <> pressure <> "\n"
        <> "Humidity: " <> humidity 

