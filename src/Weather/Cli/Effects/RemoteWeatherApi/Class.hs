-- | An effect for interacting with a remote weather API.
module Weather.Cli.Effects.RemoteWeatherApi.Class
  ( RemoteWeatherApi(..)
  , RemoteWeatherApiError(..)
  ) where

import           Relude

import           Weather.Cli.Types
import           Weather.Cli.App
import           Weather.Cli.Effects.RemoteWeatherApi.Env

import           Data.Aeson                     ( FromJSON(..)
                                                , (.:)
                                                , withObject
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.Show                       ( Show(..) )
import           Servant.API
import           Servant.Client


-- | A error that can occur while interacting with the
-- remote weather API.
data RemoteWeatherApiError
  = ZipCodeNotFound
  | UnableToReachServer
  | MalformedResponse
  | Unauthorized

instance Show RemoteWeatherApiError where
  show ZipCodeNotFound = "Unable to find the weather for requested zip code."
  show UnableToReachServer = "Unable to reach to the remote server."
  show MalformedResponse = "Response from server was not in the expected format."
  show Unauthorized = "Unauthorized. This likely due to an invalid API key."

-- | An effect for interacting with a remote weather API.
class Monad m => RemoteWeatherApi m where
  -- | Get the weather report.
  getCurrentWeather :: WeatherRequest -> m (Either RemoteWeatherApiError CurrentWeatherResponse)



instance RemoteWeatherApi MonadApp where
  getCurrentWeather WeatherRequest {..} = do
    RemoteWeatherApiEnv {..} <- asks remoteWeatherApiEnv

    let zipCode = fromUsZipCode reqUsZipCode <> ",us"
        units   = case reqMeasureUnit of
          Standard -> "standard"
          Imperial -> "imperial"
          Metric   -> "metric"
        requestAction = getWeatherHttp zipCode units remoteApiKey
        status        = fromEnum . responseStatusCode

    rawResponse <- liftIO $ runClientM requestAction remoteClientEnv

    case rawResponse of
      Left (ConnectionError _) -> pure . Left $ UnableToReachServer
      Left (FailureResponse _ r)
        | status r == 404 -> pure . Left $ ZipCodeNotFound
        | status r == 401 -> pure . Left $ Unauthorized
      Left _ -> pure . Left $ MalformedResponse
      Right responseBody ->
        case fmap head . nonEmpty $ rawWeather responseBody of
          Nothing -> pure . Left $ MalformedResponse
          Just weather ->
            let RawMain {..}    = rawMain responseBody
                RawWeather {..} = weather
            in  pure . Right $ CurrentWeatherResponse { respMain           = main
                                               , respDescription = description
                                               , respTemperature    = temp
                                               , respFeelsLike      = feelsLike
                                               , respMinTemperature = tempMin
                                               , respMaxTemperature = tempMax
                                               , respPressure       = pressure
                                               , respHumidity       = humidity
                                               }


data RawWeather = RawWeather
  { main :: Text
  , description :: Text
  }

instance FromJSON RawWeather where
  parseJSON = withObject "RawWeather" $ \v ->
    RawWeather
      <$> v .: "main"
      <*> v .: "description"

data RawMain = RawMain
  { temp :: Float
  , feelsLike :: Float
  , tempMin :: Float
  , tempMax :: Float
  , pressure :: Int
  , humidity :: Int
  }

instance FromJSON RawMain where
  parseJSON = withObject "RawMain" $ \v ->
    RawMain
      <$> v .: "temp"
      <*> v .: "feels_like"
      <*> v .: "temp_min"
      <*> v .: "temp_max"
      <*> v .: "pressure"
      <*> v .: "humidity"

data RawResponse = RawResponse
  { rawWeather :: [RawWeather]
  , rawMain :: RawMain
  }

instance FromJSON RawResponse where
  parseJSON = withObject "RawResponse" $ \v ->
    RawResponse
      <$> v .: "weather"
      <*> v .: "main"

type OpenWeatherApi = 
  "weather" 
    :> QueryParam' '[Required] "zip" Text
    :> QueryParam' '[Required] "units" Text
    :> QueryParam' '[Required] "appid" Text
    :> Get '[JSON] RawResponse

getWeatherHttp :: Text -> Text -> Text -> ClientM RawResponse
getWeatherHttp = client $ Proxy @OpenWeatherApi
