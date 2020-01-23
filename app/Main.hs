module Main where

import           Relude

--import           Weather.Cli.ApiKey
--import           Weather.Cli.App
--import           Weather.Cli.Service

import           Weather.Cli.CommandLineParser

main :: IO ()
main = getCommand >>= print
  --env <- getApiKey >>= makeEnv
  --runMonadApp env reportWeather
