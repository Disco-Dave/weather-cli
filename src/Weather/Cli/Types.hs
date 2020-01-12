-- | A module to hold public types used by the application.
module Weather.Cli.Types
  ( 
  -- * US Zip Code
    UsZipCode
  , UsZipCodeError(..)
  , makeUsZipCode
  , fromUsZipCode

  -- * Measurement Unit
  , MeasurementUnit(..)
  )
where

import           Relude

import           Data.Char                      ( isDigit )
import           Data.Maybe                     ( catMaybes )
import           Relude.Extra.Validation        ( Validation(..) )

import qualified Data.Set                      as Set
import qualified Data.Text                     as Text


-- * Validation helpers

validateValue :: Ord err => [value -> Maybe err] -> value -> Validation (Set err) value
validateValue validations value | Set.null errors = Success value
                                | otherwise       = Failure errors
  where errors = fromList . catMaybes $ fmap ($ value) validations

validation :: (value -> Bool) -> err -> value -> Maybe err
validation predicate err value | predicate value = Nothing
                               | otherwise       = Just err


-- * US Zip Code

-- | A "validated" US Zip Code.
newtype UsZipCode = UsZipCode { _fromUsZipCode :: Text }
  deriving (Show, Eq)

data UsZipCodeError
  = ContainsMoreThanDigitsUsZipCodeError
  | IsNotFiveCharactersUsZipCodeError
  deriving (Show, Eq, Ord)

-- | Make an US Zip Code from raw text.
-- It must be five characters in length, and be all numbers.
makeUsZipCode :: Text -> Validation (Set UsZipCodeError) UsZipCode
makeUsZipCode = fmap UsZipCode . validate
 where
  isFiveCharacters = (== 5) . Text.length
  isAllDigits      = Text.all isDigit
  validate         = validateValue validations . Text.strip
  validations =
    [ validation isFiveCharacters IsNotFiveCharactersUsZipCodeError
    , validation isAllDigits      ContainsMoreThanDigitsUsZipCodeError
    ]

-- | Convert an US ZIP Code to its raw text.
fromUsZipCode :: UsZipCode -> Text
fromUsZipCode = _fromUsZipCode



-- * Measurement Unit

-- | The measurement units used for the response
data MeasurementUnit
  = Standard
  | Imperial
  | Metric
  deriving (Show, Eq)
