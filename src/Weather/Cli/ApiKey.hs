module Weather.Cli.ApiKey where

import           Relude

import           System.Directory
import           System.FilePath

import qualified Data.Text                     as Text


getApiKey :: IO Text
getApiKey = do
  configDir <- (</> "weather") <$> getXdgDirectory XdgConfig ""
  let apiKeyFile = configDir </> "api_key"
  doesApiKeyExist <- doesFileExist apiKeyFile
  if doesApiKeyExist
    then Text.strip <$> readFileText apiKeyFile
    else do
      putTextLn
        "Please enter an api key. You can get one from https://openweathermap.org/"
      apiKey <- Text.strip <$> getLine
      writeFileText apiKeyFile apiKey
      pure apiKey
