module Main where

import           Relude

import           Weather.Cli.App
import           Weather.Cli.CommandLineParser
import           Weather.Cli.Service

import           System.Environment
import           System.IO


main :: IO ()
main = do
  env     <- makeEnv
  command <- getCommand
  result  <- runMonadApp env $ runCommand command
  case result of
    Left errors -> do
      hPutStrLn stderr $ toString errors
      exitFailure
    Right output -> do
      putTextLn output
      exitSuccess
 where
  getCommand =
    getArgs <&> fmap toText <&> parseArguments >>= runParseArgsResult

