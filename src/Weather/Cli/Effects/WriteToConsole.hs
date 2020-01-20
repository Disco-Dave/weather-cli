module Weather.Cli.Effects.WriteToConsole where

import           Relude

import           Weather.Cli.App

import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )


class Monad m => WriteToConsole m where
  writeLineToStdOut :: Text -> m ()
  writeLineToStdErr :: Text -> m ()

instance WriteToConsole MonadApp where
  writeLineToStdOut output = liftIO . putStrLn $ toString output
  writeLineToStdErr output = liftIO . hPutStrLn stderr $ toString output
