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

module Test.PGExpression (
    TestPGOperator(..)
  , TestPGConstant(..)
  , TestPGTypeCast(..)
  , TestPGExpression(..)

  , spec
  ) where

import Data.List (isInfixOf, intercalate)
import Data.Proxy (Proxy(..))
import Database.PostgreSQL.Simple.Bind.Parser
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary(..), sized, resize, oneof, suchThat, listOf)
import Test.Common (PGSql(..), arbitraryString, charOperator)
import Test.Utils (propParsingWorks)
import Test.PGString (TestPGString(..))
import Test.PGIdentifier (TestPGQualifiedIdentifier(..))
import Test.PGType (TestPGType(..))
import Test.PGConstant (TestPGConstant(..))


newtype TestPGOperator = TestPGOperator String deriving (Show)

instance Arbitrary TestPGOperator where
  arbitrary = TestPGOperator <$> arbitraryString charOperator `suchThat` (not . null)
    `suchThat` (not . isInfixOf "--")
    `suchThat` (not . isInfixOf "/*")
    `suchThat` (\s -> (not . (`elem` ("+-"::String)) . last $ s)
                   || (not . null . filter (`elem` ("~!@#%^&|`?"::String)) $ s))
instance PGSql TestPGOperator where
  render (TestPGOperator s) = s


data TestPGTypeCast
  = TPGTCPrefix       TestPGType                TestPGString
  | TPGTCSuffix       TestPGType                TestPGExpression
  | TPGTCAs           TestPGType                TestPGExpression
  | TPGTCFunctionCall TestPGQualifiedIdentifier TestPGExpression
  deriving (Show, Eq)

instance Arbitrary TestPGTypeCast where
  arbitrary = oneof [
      TPGTCPrefix       <$> arbitrary <*> arbitrary
    , TPGTCSuffix       <$> arbitrary <*> arbitrary
    , TPGTCAs           <$> arbitrary <*> arbitrary
    , TPGTCFunctionCall <$> arbitrary <*> arbitrary]

instance PGSql TestPGTypeCast where
  render (TPGTCPrefix       t v) = (render t) ++ " " ++ (render v)
  render (TPGTCSuffix       t v) = (render v) ++ "::" ++ (render t)
  render (TPGTCAs           t v) = "cast (" ++ (render v) ++ " as " ++ (render t) ++ ")"
  render (TPGTCFunctionCall t v) = (render t) ++ "(" ++ (render v) ++ ")"


data TestPGExpression
  = TPGEConstant TestPGConstant
  | TPGEIdentifier TestPGQualifiedIdentifier
  | TPGEFunctionInvocation TestPGQualifiedIdentifier [TestPGExpression]
  | TPGETypeCast TestPGTypeCast
  deriving (Show, Eq)

instance Arbitrary TestPGExpression where
  arbitrary = sized $ \n -> oneof [
      TPGEConstant <$> arbitrary
    , TPGEIdentifier <$> arbitrary
    , TPGEFunctionInvocation <$> arbitrary <*> (listOf $ resize (min 1 (n `div` 2)) arbitrary)
    , TPGETypeCast <$> arbitrary]

instance PGSql TestPGExpression where
  render (TPGEConstant c) = render c
  render (TPGEIdentifier n) = render n
  render (TPGEFunctionInvocation n args) = (render n) ++ "(" ++ (intercalate "," . map render $ args) ++ ")"
  render (TPGETypeCast e) = render e


spec :: Spec
spec = do
  describe "pgOperator" $ do
    propParsingWorks pgOperator (Proxy :: Proxy TestPGOperator)

  describe "pgExpression" $ do
    propParsingWorks pgExpression (Proxy :: Proxy TestPGExpression)
