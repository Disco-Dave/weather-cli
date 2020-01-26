module Weather.Cli.CommandLineParser
  ( ParseArgsResult
  , runParseArgsResult
  , parseArgsResultsToMaybe
  , parseArguments
  )
where

import           Relude

import           Weather.Cli.Types

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import qualified Data.Text                     as Text
import qualified Options.Applicative           as OptParse


-- * Command Line parser runner

newtype ParseArgsResult a = ParseArgsResults
  ( OptParse.ParserResult a
  ) deriving (Functor, Applicative, Monad)

parseArguments :: [Text] -> ParseArgsResult Command
parseArguments = coerce . execParser . fmap toString
 where
  execParser = OptParse.execParserPure prefs commandParser
  prefs      = OptParse.prefs OptParse.idm

runParseArgsResult :: ParseArgsResult a -> IO a
runParseArgsResult = OptParse.handleParseResult . coerce

parseArgsResultsToMaybe :: ParseArgsResult a -> Maybe a
parseArgsResultsToMaybe = OptParse.getParseResult . coerce


-- * Top level command parser

commandParser :: OptParse.ParserInfo Command
commandParser = OptParse.info parser infoModifer
 where
  parser          = OptParse.helper <*> OptParse.subparser commands
  descriptionText = "Get the weather report for a zip code"
  headerText      = "weather - weather report per zip code"
  infoModifer =
    OptParse.fullDesc
      <> OptParse.progDesc descriptionText
      <> OptParse.header headerText
  commands =
    OptParse.command "set" setApiKeyParser
      <> OptParse.command "current" getCurrentWeather
      <> OptParse.command "hourly" getHourlyWeather
      <> OptParse.command "daily" getDailyWeather



-- * Sub Parsers

setApiKeyParser :: OptParse.ParserInfo Command
setApiKeyParser = OptParse.info (OptParse.helper <*> parser) infoModifier
 where
  parser       = SetApiKey <$> OptParse.argument OptParse.str modifier
  modifier     = OptParse.metavar "API_KEY" <> OptParse.help helpText
  infoModifier = OptParse.fullDesc <> OptParse.progDesc descriptionText
  helpText =
    "The API key associated with your account "
      <> "registered on https://openweathermap.org/"
  descriptionText =
    "Set the API key associated with your account "
      <> "registered on https://openweathermap.org/"

getCurrentWeather :: OptParse.ParserInfo Command
getCurrentWeather = weatherReport GetCurrentWeather "Get the current weather"

getHourlyWeather :: OptParse.ParserInfo Command
getHourlyWeather = weatherReport GetHourlyWeather "Get the hourly weather"

getDailyWeather :: OptParse.ParserInfo Command
getDailyWeather = weatherReport GetDailyWeather "Get the daily weather"

weatherReport :: (WeatherRequest -> a) -> String -> OptParse.ParserInfo a
weatherReport constructor description = OptParse.info parser infoModifer
 where
  parser      = OptParse.helper <*> fmap constructor weatherRequestParser
  infoModifer = OptParse.fullDesc <> OptParse.progDesc description

weatherRequestParser :: OptParse.Parser WeatherRequest
weatherRequestParser =
  WeatherRequest <$> measurementUnitOption <*> usZipCodeArguement
 where
  usZipCodeArguement = OptParse.argument usZipCode usZipCodeModifier
  usZipCodeModifier =
    OptParse.metavar "ZIPCODE" <> OptParse.help "United States zip code"
  measurementUnitOption =
    OptParse.option measurementUnit measurementUnitModifier
  measurementUnitModifier =
    OptParse.long "unit"
      <> OptParse.short 'u'
      <> OptParse.metavar "UNIT"
      <> OptParse.value Imperial
      <> OptParse.showDefault
      <> OptParse.help "Measurement unit. Imperial, standard, or metric."




-- * Custom 'ReadM' values

measurementUnit :: OptParse.ReadM MeasurementUnit
measurementUnit = do
  rawMeasurementUnit <- Text.map Char.toLower <$> OptParse.str
  case rawMeasurementUnit of
    "imperial" -> pure Imperial
    "standard" -> pure Standard
    "metric"   -> pure Metric
    _          -> fail
      "Unknown measurement unit. Options are imperial, standard, or metric."

usZipCode :: OptParse.ReadM UsZipCode
usZipCode = do
  rawUsZipCode <- fmap makeUsZipCode OptParse.str
  case rawUsZipCode of
    Success validZipCode -> pure validZipCode
    Failure errors -> fail . List.unwords . fmap toErrorMessage $ toList errors
 where
  toErrorMessage ContainsMoreThanDigitsUsZipCodeError =
    "Zip code may only contain digits."
  toErrorMessage IsNotFiveCharactersUsZipCodeError =
    "Zip code must be exactly five digits of length."
