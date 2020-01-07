module Weather.Cli.App where

import           Relude


newtype MonadApp a = MonadApp { fromMonadApp :: Identity a }
  deriving (Show, Functor, Applicative, Monad)
