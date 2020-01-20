module Main where

import           Relude

import           Weather.Cli.App
import           Weather.Cli.Service
import           Weather.Cli.Types

main :: IO ()
main = do
  env <- makeEnv "4cc585a384b1eb0fd178809926eddf68"
  case makeUsZipCode "17050" of
    Failure _ -> pure ()
    Success usZipCode ->
      let req = WeatherRequest usZipCode Imperial
      in  runMonadApp env (reportWeather req)
