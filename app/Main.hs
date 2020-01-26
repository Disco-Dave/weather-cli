module Main where

import           Relude

import           Weather.Cli.App
import           Weather.Cli.CommandLineParser
import           Weather.Cli.Service

import           System.Environment             ( getArgs )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )


main :: IO ()
main = do
  env     <- makeEnv
  command <- fmap runCommand getCommand
  runMonadApp env command >>= handleResult
 where
  handleResult result = case result of
    Left  errors -> hPutStrLn stderr (toString errors) *> exitFailure
    Right output -> putTextLn output *> exitSuccess
  getCommand =
    getArgs 
      <&> fmap toText 
      <&> parseArguments 
      >>= runParseArgsResult
