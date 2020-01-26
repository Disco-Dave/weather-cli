module Weather.Cli.Effects.ApiKey
  ( GetApiKey(..)
  , SetApiKey(..)
  )
where

import           Relude

import           Weather.Cli.App

import           System.FilePath                ( (</>) )

import qualified System.Directory              as Directory


class Monad m => SetApiKey m where
  setApiKey :: Text -> m ()

instance SetApiKey MonadApp where
  setApiKey = setApiKeyIO


class Monad m => GetApiKey m where
  getApiKey :: m (Maybe Text)

instance GetApiKey MonadApp where
  getApiKey = getApiKeyIO


apiKeyFileName :: FilePath
apiKeyFileName = "api_key"

configDir :: MonadIO m => m FilePath
configDir = do
  xdgConfigDir <- liftIO $ Directory.getXdgDirectory Directory.XdgConfig ""
  pure $ xdgConfigDir </> "weather"

setApiKeyIO :: MonadIO m => Text -> m ()
setApiKeyIO apiKey = liftIO $ do
  configDir'         <- configDir
  doesConfigDirExist <- Directory.doesDirectoryExist configDir'
  unless doesConfigDirExist $ 
    Directory.createDirectoryIfMissing True configDir'
  writeFileText (configDir' </> apiKeyFileName) apiKey

getApiKeyIO :: MonadIO m => m (Maybe Text)
getApiKeyIO = liftIO $ do
  filePath      <- (</> apiKeyFileName) <$> configDir
  doesFileExist <- Directory.doesFileExist filePath
  if doesFileExist 
     then Just <$> readFileText filePath 
     else pure Nothing
