module Weather.Cli.App where

import           Relude


newtype Env = Env { apiKey :: Text } deriving Show

newtype MonadApp a = MonadApp { fromMonadApp :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)
