{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.PGArgument (
    TestPGArgument(..)
  , TestPGArgumentList(..)
  , TPGALCorrect(..)
  , TPGALFailedCheckExpectedDefaults(..)
  , TPGALFailedCheckNotExpectedDefaults(..)
  , TPGALFailedCheckVariadic(..)

  , spec
  ) where

import Data.List (intercalate, tails)
import Data.Maybe (fromMaybe, catMaybes, isJust, isNothing)
import Data.Proxy (Proxy(..))
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGArgumentClass(..), PGArgumentMode(..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary(..), sized, resize, suchThat, listOf, listOf1, elements, arbitraryBoundedEnum)
import Test.Common (PGSql(..))
import Test.Utils (propParsingWorks, propParsingFails)
import Test.PGIdentifier (TestPGIdentifier(..))
import Test.PGType (TestPGType(..))
import Test.PGExpression (TestPGExpression(..))


instance Arbitrary PGArgumentMode where
  arbitrary = arbitraryBoundedEnum

data TestPGArgument = TestPGArgument {
    tpgaMode            :: Maybe PGArgumentMode
  , tpgaName            :: Maybe TestPGIdentifier
  , tpgaType            :: TestPGType
  , tpgaDefaultNotation :: String
  , tpgaDefaultValue    :: Maybe TestPGExpression
  } deriving (Show, Eq)

instance Arbitrary TestPGArgument where
  arbitrary = do
    tpgaMode            <- arbitrary
    tpgaName            <- arbitrary
    tpgaType            <- arbitrary
    tpgaDefaultNotation <- elements ["=", "default"]
    tpgaDefaultValue    <- arbitrary
    return TestPGArgument {..}

instance PGSql TestPGArgument where
  render (TestPGArgument {..}) = concat . catMaybes $ [
      ((++ " ") . show) <$> tpgaMode
    , (++ " ") . render <$> tpgaName
    , Just . render $ tpgaType
    , (' ':)  . (tpgaDefaultNotation ++) . (' ':) . render <$> tpgaDefaultValue]

instance PGArgumentClass TestPGArgument TestPGType where
  argumentMode     = fromMaybe In . tpgaMode
  argumentOptional = maybe False (const True) . tpgaDefaultValue
  argumentType     = tpgaType


newtype TestPGArgumentList = TestPGArgumentList { getArguments :: [TestPGArgument] } deriving (Show)

instance Arbitrary TestPGArgumentList where
  arbitrary = TestPGArgumentList <$> (listOf arbitrary)
  shrink (TestPGArgumentList xs) = map TestPGArgumentList (tail . tails $ xs)

instance PGSql TestPGArgumentList where
  render (TestPGArgumentList xs) = intercalate ", " . map render $ xs


wrap :: ArgumentListChecker TestPGArgument -> TestPGArgumentList -> Bool
wrap check (TestPGArgumentList xs) = either (const False) (const True) . check $ xs

newtype TPGALCorrect = TPGALCorrect { getArgumentList :: TestPGArgumentList } deriving (Show)

instance Arbitrary TPGALCorrect where
  arbitrary = TPGALCorrect <$> arbitrary
    `suchThat` (wrap checkExpectedDefaults)
    `suchThat` (wrap checkNotExpectedDefaults)
    `suchThat` (wrap checkVariadic)

instance PGSql TPGALCorrect where
  render (TPGALCorrect x) = render x


newtype TPGALFailedCheckExpectedDefaults = TPGALFailedCheckExpectedDefaults TestPGArgumentList deriving (Show)

instance Arbitrary TPGALFailedCheckExpectedDefaults where
  arbitrary = TPGALFailedCheckExpectedDefaults <$> arbitraryArgumentList
    `suchThat` (not . wrap checkExpectedDefaults)
    `suchThat` (wrap checkNotExpectedDefaults)
    `suchThat` (wrap checkVariadic)
    where
      arbitraryArgumentList = sized $ \n -> fmap (TestPGArgumentList . concat)
                                          . sequence . map (resize $ max 1 (n `div` 3)) $ [
          listOf  arbitrary
        , listOf1 (arbitrary `suchThat` (isJust . tpgaDefaultValue)) `suchThat` (not . empty')
        , listOf1 (arbitrary `suchThat` (isNothing . tpgaDefaultValue)) `suchThat` (not . empty')]

      empty' = null . filter ((`elem` [In, InOut]) . fromMaybe In  . tpgaMode)

instance PGSql TPGALFailedCheckExpectedDefaults where
  render (TPGALFailedCheckExpectedDefaults x) = render x


newtype TPGALFailedCheckNotExpectedDefaults = TPGALFailedCheckNotExpectedDefaults TestPGArgumentList deriving (Show)

instance Arbitrary TPGALFailedCheckNotExpectedDefaults where
  arbitrary = TPGALFailedCheckNotExpectedDefaults <$> arbitrary
    `suchThat` (wrap checkExpectedDefaults)
    `suchThat` (not . wrap checkNotExpectedDefaults)
    `suchThat` (wrap checkVariadic)

instance PGSql TPGALFailedCheckNotExpectedDefaults where
  render (TPGALFailedCheckNotExpectedDefaults x) = render x


newtype TPGALFailedCheckVariadic = TPGALFailedCheckVariadic TestPGArgumentList deriving (Show)

instance Arbitrary TPGALFailedCheckVariadic where
  arbitrary = TPGALFailedCheckVariadic <$> arbitrary
    `suchThat` (wrap checkExpectedDefaults)
    `suchThat` (wrap checkNotExpectedDefaults)
    `suchThat` (not . wrap checkVariadic)

instance PGSql TPGALFailedCheckVariadic where
  render (TPGALFailedCheckVariadic x) = render x


spec :: Spec
spec = do
  describe "pgArgument" $ do
    propParsingWorks pgArgument (Proxy :: Proxy TestPGArgument)

  describe "pgArgumentList" $ do
    propParsingWorks (pgArgumentList True) (Proxy :: Proxy TPGALCorrect)
    propParsingFails (pgArgumentList True) "DefaultValueExpected" (Proxy :: Proxy TPGALFailedCheckExpectedDefaults)
    propParsingFails (pgArgumentList True) "DefaultValueNotExpected" (Proxy :: Proxy TPGALFailedCheckNotExpectedDefaults)
    propParsingFails (pgArgumentList True) "NonOutVariableAfterVariadic" (Proxy :: Proxy TPGALFailedCheckVariadic)
