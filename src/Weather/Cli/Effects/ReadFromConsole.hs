module Weather.Cli.Effects.ReadFromConsole
  ( ReadFromConsole(..)
  )
where

import           Relude

import           Weather.Cli.App
import           Weather.Cli.Types

import           Options.Applicative

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import qualified Data.Text                     as Text
import qualified Relude.Extra.Validation       as Validation


class Monad m => ReadFromConsole m where
  getRequest :: m WeatherRequest

instance ReadFromConsole MonadApp where
  getRequest = liftIO . execParser $ weatherRequestParserInfo


weatherRequestParserInfo :: ParserInfo WeatherRequest
weatherRequestParserInfo = info
  (helper <*> weatherRequestParser)
  (fullDesc <> progDesc "Get current weather by zip code." <> header
    "weather - get current weather report."
  )

weatherRequestParser :: Parser WeatherRequest
weatherRequestParser =
  WeatherRequest
    <$> option
          measurementUnit
          (long "unit" 
            <> showDefault
            <> short 'u' 
            <> value Imperial 
            <> help "Measurement unit. Imperial, standard, or metric."
          )
    <*> argument usZipCode
                 (metavar "zipcode" <> help "United States zip code.")

measurementUnit :: ReadM MeasurementUnit
measurementUnit = do
  rawMeasurementUnit <- Text.map Char.toLower <$> str
  case rawMeasurementUnit of
    "imperial" -> pure Imperial
    "standard" -> pure Standard
    "metric"   -> pure Metric
    _          -> fail
      "Unknown measurement unit. Options are imperial, standard, or metric."

usZipCode :: ReadM UsZipCode
usZipCode = do
  rawUsZipCode <- fmap makeUsZipCode str
  case rawUsZipCode of
    Validation.Success validZipCode -> pure validZipCode
    Validation.Failure errors ->
      fail . List.unwords . fmap toErrorMessage $ toList errors
 where
  toErrorMessage ContainsMoreThanDigitsUsZipCodeError =
    "Zip code may only contain digits."
  toErrorMessage IsNotFiveCharactersUsZipCodeError =
    "Zip code must be exactly five digits of length."
