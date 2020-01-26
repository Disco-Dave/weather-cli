-- | An effect for interacting with a remote weather API.
module Weather.Cli.Effects.RemoteWeatherApi.Class
  ( RemoteWeatherApi(..)
  , RemoteWeatherApiError(..)
  ) where

import           Relude

import           Weather.Cli.App
import           Weather.Cli.Effects.ApiKey
import           Weather.Cli.Effects.RemoteWeatherApi.Env

import           Data.Aeson
import           Data.Proxy                     ( Proxy(..) )
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
  | ApiKeyMissing

-- | An effect for interacting with a remote weather API.
class Monad m => RemoteWeatherApi m where
  -- | Get the weather report.
  getCurrentWeather :: Types.WeatherRequest -> m (Either RemoteWeatherApiError Types.CurrentWeatherResponse)



instance RemoteWeatherApi MonadApp where
  getCurrentWeather Types.WeatherRequest {..} =
    let zipCode = UsZipCode reqUsZipCode
        units   = MeasurementUnit reqMeasureUnit
    in  runHttp $ getWeatherHttp zipCode units


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

instance ToHttpApiData ApiKey where
  toUrlPiece (ApiKey appId) = appId

newtype ApiKey = ApiKey Text

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
        pure . CurrentWeatherResponse $ response


type OpenWeatherApi =
  "weather"
    :> QueryParam' '[Required] "zip" UsZipCode
    :> QueryParam' '[Required] "units" MeasurementUnit
    :> QueryParam' '[Required] "appid" ApiKey
    :> Get '[JSON] CurrentWeatherResponse

getWeatherHttp :: UsZipCode -> MeasurementUnit -> ApiKey -> ClientM CurrentWeatherResponse
getWeatherHttp = client $ Proxy @OpenWeatherApi

runHttp
  :: (MonadIO m, GetApiKey m, HasRemoteWeatherApiEnv m, Coercible a b)
  => (ApiKey -> ClientM a)
  -> m (Either RemoteWeatherApiError b)
runHttp action = do
  RemoteWeatherApiEnv {..} <- getRemoteWeatherApiEnv
  maybeApiKey              <- getApiKey
  case maybeApiKey of
    Nothing      -> pure . Left $ ApiKeyMissing
    Just apiKey' -> do
      let action' = action (ApiKey apiKey')
      rawResponse <- liftIO $ runClientM action' remoteClientEnv
      pure $ case rawResponse of
        Left (ConnectionError _) -> Left UnableToReachServer
        Left (FailureResponse _ r) | status r == 404 -> Left ZipCodeNotFound
                                   | status r == 401 -> Left Unauthorized
        Left  _            -> Left MalformedResponse
        Right responseBody -> Right $ coerce responseBody
  where status = fromEnum . responseStatusCode
