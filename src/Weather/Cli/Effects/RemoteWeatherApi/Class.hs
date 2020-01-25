-- | An effect for interacting with a remote weather API.
module Weather.Cli.Effects.RemoteWeatherApi.Class
  ( RemoteWeatherApi(..)
  , RemoteWeatherApiError(..)
  ) where

import           Relude

import           Weather.Cli.App
import           Weather.Cli.Effects.RemoteWeatherApi.Env


import           Data.Aeson
import           Data.Proxy                     ( Proxy(..) )
import           GHC.Show                       ( Show(..) )
import           Servant.API
import           Servant.Client

import qualified Weather.Cli.Types             as Types


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
  getCurrentWeather :: Types.WeatherRequest -> m (Either RemoteWeatherApiError Types.CurrentWeatherResponse)



instance RemoteWeatherApi MonadApp where
  getCurrentWeather Types.WeatherRequest {..} = do
    RemoteWeatherApiEnv {..} <- asks remoteWeatherApiEnv

    let zipCode       = UsZipCode reqUsZipCode
        units         = MeasurementUnit reqMeasureUnit
        requestAction = getWeatherHttp zipCode units (AppId remoteApiKey)
        status        = fromEnum . responseStatusCode

    rawResponse <- liftIO $ runClientM requestAction remoteClientEnv

    case rawResponse of
      Left (ConnectionError _) -> pure . Left $ UnableToReachServer
      Left (FailureResponse _ r)
        | status r == 404 -> pure . Left $ ZipCodeNotFound
        | status r == 401 -> pure . Left $ Unauthorized
      Left _ -> pure . Left $ MalformedResponse
      Right (CurrentWeatherResponse responseBody) ->
        pure . Right $ responseBody


newtype UsZipCode = UsZipCode Types.UsZipCode

instance ToHttpApiData UsZipCode where
  toUrlPiece (UsZipCode zipCode) = rawZipCode <> ",us"
    where rawZipCode = Types.fromUsZipCode zipCode


newtype MeasurementUnit = MeasurementUnit Types.MeasurementUnit

instance ToHttpApiData MeasurementUnit where
  toUrlPiece (MeasurementUnit measurementUnit) = case measurementUnit of
    Types.Metric   -> "metric"
    Types.Imperial -> "imperial"
    Types.Standard -> "standard"

instance ToHttpApiData AppId where
  toUrlPiece (AppId appId) = appId

newtype AppId = AppId Text

newtype CurrentWeatherResponse = CurrentWeatherResponse Types.CurrentWeatherResponse


instance FromJSON CurrentWeatherResponse where
  parseJSON = withObject "CurrentWeatherResponse" $ \value -> do
    rawWeatherArray <- value .: "weather"
    rawMain         <- value .: "main"
    case rawWeatherArray of
      []               -> fail "No weather information was sent"
      (rawWeather : _) -> do
        response <- Types.CurrentWeatherResponse
          <$> rawWeather .: "main"
          <*> rawWeather .: "description"
          <*> rawMain    .: "temp"
          <*> rawMain    .: "feels_like"
          <*> rawMain    .: "temp_min"
          <*> rawMain    .: "temp_max"
          <*> rawMain    .: "pressure" 
          <*> rawMain    .: "humidity" 
        pure . coerce $ response


type OpenWeatherApi =
  "weather"
    :> QueryParam' '[Required] "zip" UsZipCode
    :> QueryParam' '[Required] "units" MeasurementUnit
    :> QueryParam' '[Required] "appid" AppId
    :> Get '[JSON] CurrentWeatherResponse

getWeatherHttp :: UsZipCode -> MeasurementUnit -> AppId -> ClientM CurrentWeatherResponse
getWeatherHttp = client $ Proxy @OpenWeatherApi
