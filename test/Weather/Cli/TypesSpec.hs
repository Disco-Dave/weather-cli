{-# OPTIONS_GHC -fno-warn-orphans #-}

module Weather.Cli.TypesSpec where

import           Relude

import           Weather.Cli.Types

import           Data.Char                      ( isDigit )
import           Relude.Extra.Validation
import           Test.Hspec
import           Test.QuickCheck         hiding ( Failure
                                                , Success
                                                )

import qualified Data.Set                      as Set
import qualified Data.Text                     as Text


spec :: Spec
spec = describe "US Zip Code" usZipCodeSpec


usZipCodeSpec :: Spec
usZipCodeSpec = do
  it "can make valid 5 digit zip code"
    $ forAll validUsZipCodes
    $ \rawUsZipCode -> case makeUsZipCode rawUsZipCode of
        Failure _         -> expectationFailure "Should of been valid."
        Success usZipCode -> fromUsZipCode usZipCode `shouldBe` rawUsZipCode

  it "works with valid zip codes with leading or tailing spaces"
    $ forAll (addWhiteSpace validUsZipCodes)
    $ \rawUsZipCode -> case makeUsZipCode rawUsZipCode of
        Failure _ -> expectationFailure "Should of been valid."
        Success usZipCode ->
          fromUsZipCode usZipCode `shouldBe` Text.strip rawUsZipCode

  it "marks zip codes that aren't exactly 5 characters as invalid"
    $ forAll (notLengthOf 5 digitsOfText)
    $ \rawUsZipCode -> case makeUsZipCode rawUsZipCode of
        Success _    -> expectationFailure "Should of been invalid."
        Failure errs -> do
          errs `shouldSatisfy` Set.member IsNotFiveCharactersUsZipCodeError
          errs `shouldSatisfy` (== 1) . Set.size

  it "marks zip codes that aren't all digits as invalid"
    $ forAll (textOfLength 5 `suchThat` Text.any (not . isDigit))
    $ \rawUsZipCode -> case makeUsZipCode rawUsZipCode of
        Success _    -> expectationFailure "Should of been invalid."
        Failure errs -> do
          errs `shouldSatisfy` Set.member ContainsMoreThanDigitsUsZipCodeError
          errs `shouldSatisfy` (== 1) . Set.size

  it "marks zip codes that aren't all digits, and aren't 5 characters as invalid"
    $ let generator = arbitrary `suchThat` (\t -> containsNotDigits t && isIncorrectLength t)
          containsNotDigits = Text.any (not . isDigit)
          isIncorrectLength = (/= 5) . Text.length . Text.strip
      in  forAll generator $ \rawUsZipCode -> case makeUsZipCode rawUsZipCode of
            Success _    -> expectationFailure "Should of been invalid."
            Failure errs -> do
              errs `shouldSatisfy` Set.member ContainsMoreThanDigitsUsZipCodeError
              errs `shouldSatisfy` Set.member IsNotFiveCharactersUsZipCodeError
              errs `shouldSatisfy` (== 2) . Set.size



notLengthOf :: forall a . Int -> (Int -> Gen a) -> Gen a
notLengthOf notLength newGen = do
  i <- arbitrary `suchThat` (/= notLength)
  newGen i

validUsZipCodes :: Gen Text
validUsZipCodes = digitsOfText 5

digitsOfText :: Int -> Gen Text
digitsOfText textLength = toText <$> vectorOf textLength digits
  where digits = elements ['0' .. '9']

addWhiteSpace :: Gen Text -> Gen Text
addWhiteSpace genText = do
  trailing <- whiteSpace
  leading  <- whiteSpace
  text     <- genText
  return $ leading <> text <> trailing
  where whiteSpace = toText <$> listOf (pure ' ')

instance Arbitrary Text where
  arbitrary = toText <$> arbitrary @String

textOfLength :: Int -> Gen Text
textOfLength size =
  fmap Text.strip arbitrary `suchThat` ((== size) . Text.length)
