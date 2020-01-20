module Main where

import           Relude

import           Weather.Cli.ApiKey
import           Weather.Cli.App
import           Weather.Cli.Service

main :: IO ()
main = do
  env <- getApiKey >>= makeEnv
  runMonadApp env reportWeather
