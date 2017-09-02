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

module Test.PGIdentifier (
    TestPGNormalIdentifier(..)
  , TestPGIdentifier(..)
  , TestPGQualifiedIdentifier(..)

  , spec
  ) where

import Data.Proxy (Proxy(..))
import Data.Tagged (tagWith)
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGIdentifier(..))
import Test.Hspec (Spec, describe, shouldSatisfy, shouldBe)
import Test.QuickCheck (Gen, Arbitrary(..), oneof, suchThat)
import qualified Data.Text as T
import Test.Common (PGSql(..), arbitraryString', charId')
import Test.Utils (propParserRight, propParsingWorks)
import Test.PGString (TestPGQuotedString(..))


newtype TestPGNormalIdentifier = TestPGNormalIdentifier String deriving (Show)

instance Arbitrary TestPGNormalIdentifier where
  arbitrary = TestPGNormalIdentifier <$> (arbitraryString' charId') `suchThat` (not . null)

instance PGSql TestPGNormalIdentifier where
  render (TestPGNormalIdentifier s) = s


newtype TestPGIdentifier = TestPGIdentifier String deriving (Show, Eq)

instance Arbitrary TestPGIdentifier where
  arbitrary = TestPGIdentifier <$> oneof [
      render <$> (arbitrary :: Gen TestPGNormalIdentifier)
    , (('"':) . render <$> (arbitrary :: Gen (TestPGQuotedString "\""))) `suchThat` ((> 2) . length)]

instance PGSql TestPGIdentifier where
  render (TestPGIdentifier s) = s


newtype TestPGQualifiedIdentifier = TestPGQualifiedIdentifier PGIdentifier deriving (Show, Eq)

instance Arbitrary TestPGQualifiedIdentifier where
  arbitrary = do
    pgiSchema <- oneof [return Nothing, Just . render <$> (arbitrary :: Gen TestPGIdentifier)]
    pgiName   <- render <$> (arbitrary :: Gen TestPGIdentifier)
    return $ TestPGQualifiedIdentifier PGIdentifier {..}

instance PGSql TestPGQualifiedIdentifier where
  render (TestPGQualifiedIdentifier PGIdentifier {..}) = maybe pgiName (++ ('.':pgiName)) pgiSchema


spec :: Spec
spec = do
  describe "pgNormalIdentifier" $ do
    propParsingWorks pgNormalIdentifier (Proxy :: Proxy TestPGNormalIdentifier)
    let prop' = propParserRight (tagWith (Proxy :: Proxy TestPGNormalIdentifier) pgNormalIdentifier)
    prop' "the first symbol is not '$'" . flip shouldSatisfy $ \s -> (T.head s) /= '$'
    prop' "stored in lowercase" $ \x -> x `shouldBe` (T.toLower x)

  describe "pgIdentifier" $ do
    propParsingWorks pgIdentifier (Proxy :: Proxy TestPGIdentifier)

  describe "pgQualifiedIdentifier" $ do
    propParsingWorks pgQualifiedIdentifier (Proxy :: Proxy TestPGQualifiedIdentifier)
