module Weather.Cli.App where

import           Relude


newtype MonadApp a = MonadApp { fromMonadApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
