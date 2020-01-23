module Weather.Cli.CommandLineParser
  ( getCommand
  , parseArguments
  )
where

import           Relude

import           Weather.Cli.Types

import           Options.Applicative            ( Parser
                                                , ParserInfo
                                                , ReadM
                                                )

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import qualified Data.Text                     as Text
import qualified Options.Applicative           as OptParse


-- * Command Line parser runners

getCommand :: MonadIO m => m Command
getCommand = liftIO $ OptParse.execParser commandParser

parseArguments :: [Text] -> OptParse.ParserResult Command
parseArguments = OptParse.execParserPure prefs commandParser . fmap toString
  where prefs = OptParse.prefs mempty




-- * Top level command parser

commandParser :: ParserInfo Command
commandParser = OptParse.info parser infoModifer
 where
  parser          = OptParse.helper <*> OptParse.subparser commands
  descriptionText = "Get the weather report for a zip code"
  headerText      = "weather - weather report per zip code"
  commands =
    OptParse.command "set" setApiKeyParser
      <> OptParse.command "current" getCurrentWeather
  infoModifer =
    OptParse.fullDesc
      <> OptParse.progDesc descriptionText
      <> OptParse.header headerText



-- * Sub Parsers

setApiKeyParser :: ParserInfo Command
setApiKeyParser = OptParse.info parser infoModifier
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

getCurrentWeather :: ParserInfo Command
getCurrentWeather = OptParse.info parser infoModifer
 where
  parser          = fmap GetCurrentWeather weatherRequestParser
  infoModifer     = OptParse.fullDesc <> OptParse.progDesc descriptionText
  descriptionText = "Get the current weather"

weatherRequestParser :: Parser WeatherRequest
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

measurementUnit :: ReadM MeasurementUnit
measurementUnit = do
  rawMeasurementUnit <- Text.map Char.toLower <$> OptParse.str
  case rawMeasurementUnit of
    "imperial" -> pure Imperial
    "standard" -> pure Standard
    "metric"   -> pure Metric
    _          -> fail
      "Unknown measurement unit. Options are imperial, standard, or metric."

usZipCode :: ReadM UsZipCode
usZipCode = do
  rawUsZipCode <- fmap makeUsZipCode OptParse.str
  case rawUsZipCode of
    Success validZipCode -> pure validZipCode
    Failure errors ->
      fail . List.unwords . fmap toErrorMessage $ toList errors
 where
  toErrorMessage ContainsMoreThanDigitsUsZipCodeError =
    "Zip code may only contain digits."
  toErrorMessage IsNotFiveCharactersUsZipCodeError =
    "Zip code must be exactly five digits of length."
