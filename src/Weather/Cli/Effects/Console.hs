module Weather.Cli.Effects.Console where

import           Relude

import           Weather.Cli.App

import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )


class Monad m => Console m where
  readFromStdIn :: m Text
  writeLineToStdOut :: Text -> m ()
  writeLineToStdErr :: Text -> m ()

readValueFromStdIn :: (Console m, Read a) => m (Maybe a)
readValueFromStdIn = fmap (readMaybe . toString) readFromStdIn

instance Console MonadApp where
  readFromStdIn = getLine
  writeLineToStdOut output = liftIO . putStrLn $ toString output
  writeLineToStdErr output = liftIO . hPutStrLn stderr $ toString output
