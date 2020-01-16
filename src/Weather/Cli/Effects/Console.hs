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

instance Console IO where
  readFromStdIn = getLine
  writeLineToStdOut output = putStrLn (toString output)
  writeLineToStdErr output = hPutStrLn stderr (toString output)

instance Console MonadApp where
  readFromStdIn = liftIO readFromStdIn
  writeLineToStdOut = liftIO . writeLineToStdOut
  writeLineToStdErr = liftIO . writeLineToStdErr
