module Main where

import           Relude

--import           Weather.Cli.ApiKey
--import           Weather.Cli.App
--import           Weather.Cli.Service

import           Weather.Cli.CommandLineParser

import           System.Environment

getArgsText :: IO [Text]
getArgsText = fmap toText <$> getArgs

main :: IO ()
main = getArgsText >>= (runParseArgsResult . parseArguments) >>= print
  --
  --env <- getApiKey >>= makeEnv
  --runMonadApp env reportWeather
