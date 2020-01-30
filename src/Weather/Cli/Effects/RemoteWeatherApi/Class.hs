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
import           GHC.TypeLits
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
  getCurrentWeather :: Types.WeatherRequest -> m (Either RemoteWeatherApiError Types.WeatherReport)
  getHourlyWeather :: Types.WeatherRequest -> m (Either RemoteWeatherApiError Types.HourlyWeatherResponse)
  getDailyWeather :: Types.WeatherRequest -> m (Either RemoteWeatherApiError Types.DailyWeatherResponse)

instance RemoteWeatherApi MonadApp where
  getCurrentWeather = getWeather getCurrentHttp
  getHourlyWeather  = getWeather getHourlyHttp
  getDailyWeather   = getWeather getDailyHttp



-- * Newtypes and type classes for API

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

newtype ApiKey = ApiKey Text
instance ToHttpApiData ApiKey where
  toUrlPiece (ApiKey apiKey) = apiKey

newtype WeatherReport = WeatherReport {fromWeatherReport :: Types.WeatherReport}
instance FromJSON WeatherReport where
  parseJSON = withObject "WeatherReport" $ \obj -> do
    rawWeatherArray <- obj .: "weather"
    rawMain         <- obj .: "main"
    case rawWeatherArray of
      []               -> fail "No weather information was sent"
      (rawWeather : _) -> do
        response <- Types.WeatherReport
          <$> rawWeather .: "main"
          <*> rawWeather .: "description"
          <*> rawMain    .: "temp"
          <*> rawMain    .: "feels_like"
          <*> rawMain    .: "temp_min"
          <*> rawMain    .: "temp_max"
          <*> rawMain    .: "pressure"
          <*> rawMain    .: "humidity"
        pure . WeatherReport $ response

newtype HourlyWeatherResponse = HourlyWeatherResponse Types.HourlyWeatherResponse
instance FromJSON HourlyWeatherResponse where
  parseJSON = withObject "HourlyWeatherResponse" $ \obj -> do
    resp <-
      Types.HourlyWeatherResponse
        <$> obj .:  "dt"
        <*> (fromWeatherReport <$> parseJSON (Object obj))
    pure . HourlyWeatherResponse $ resp

instance {-# OVERLAPPING#-} FromJSON [HourlyWeatherResponse] where
  parseJSON = withObject "HourlyWeatherResponse" $ \obj -> obj .: "list"


newtype DailyWeatherResponse = DailyWeatherResponse Types.DailyWeatherResponse
instance FromJSON DailyWeatherResponse where
  parseJSON = withObject "DailyWeatherResponse" $ \_ ->
    pure $ DailyWeatherResponse Types.DailyWeatherResponse



-- * Open Weather API

type OpenWeatherApi =
  Weather "weather" WeatherReport
  :<|> "forecast" :>
    ( Weather "hourly" HourlyWeatherResponse
    :<|> Weather "daily" DailyWeatherResponse
    )

getCurrentHttp :: UsZipCode -> MeasurementUnit -> ApiKey -> ClientM WeatherReport
getHourlyHttp :: UsZipCode -> MeasurementUnit -> ApiKey -> ClientM HourlyWeatherResponse
getDailyHttp :: UsZipCode -> MeasurementUnit -> ApiKey -> ClientM DailyWeatherResponse
(getCurrentHttp :<|> getHourlyHttp :<|> getDailyHttp) = client $ Proxy @OpenWeatherApi


-- * Helpers

type Weather (name :: Symbol) (response :: Type) =
  name
    :> QueryParam' '[Required] "zip" UsZipCode
    :> QueryParam' '[Required] "units" MeasurementUnit
    :> QueryParam' '[Required] "appid" ApiKey
    :> Get '[JSON] response

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
        Left (FailureResponse _ r) | status r == 404 -> traceShow r $ Left ZipCodeNotFound
                                   | status r == 401 -> traceShow r $ Left Unauthorized
        Left  _            -> Left MalformedResponse
        Right responseBody -> Right $ coerce responseBody
  where status = fromEnum . responseStatusCode

getWeather
  :: (MonadIO m, GetApiKey m, HasRemoteWeatherApiEnv m, Coercible a b)
  => (UsZipCode -> MeasurementUnit -> ApiKey -> ClientM a)
  -> Types.WeatherRequest
  -> m (Either RemoteWeatherApiError b)
getWeather action Types.WeatherRequest {..} = runHttp $ action zipCode units
 where
  zipCode = UsZipCode reqUsZipCode
  units   = MeasurementUnit reqMeasureUnit
