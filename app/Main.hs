module Main where

import           Relude

import           Weather.Cli.App
import           Weather.Cli.Service

main :: IO ()
main = do
  env <- makeEnv "4cc585a384b1eb0fd178809926eddf68"
  runMonadApp env reportWeather
