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

module Test.PGType (
    TestPGColumnType(..)
  , TestPGExactType(..)
  , TestPGType(..)

  , spec
  ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGTypeClass(..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Gen, Arbitrary(..), oneof, arbitrarySizedNatural, listOf, elements)
import Test.Common (PGSql(..))
import Test.Utils (propParsingWorks)
import Test.PGIdentifier (TestPGIdentifier(..), TestPGQualifiedIdentifier(..))
import Test.PGConstant (TestPGConstant(..))


newtype TestPGColumnType = TestPGColumnType String deriving (Show)

instance Arbitrary TestPGColumnType where
  arbitrary = TestPGColumnType <$> do
    tableName  <- render <$> (arbitrary :: Gen TestPGIdentifier)
    columnName <- render <$> (arbitrary :: Gen TestPGIdentifier)
    return $ concat [tableName, ".", columnName, "%type"]

instance PGSql TestPGColumnType where
  render (TestPGColumnType s) = s


data TestPGExactType = TestPGExactType {
    tpgetName       :: String
  , tpgetModifiers  :: [String]
  , tpgetTimeZone   :: String
  , tpgetDimensions :: String
  } deriving (Show)

instance Arbitrary TestPGExactType where
  arbitrary = do
    tpgetName       <- arbitraryTypeName
    tpgetModifiers  <- listOf arbitraryTypeModifier
    tpgetTimeZone   <- if (map toLower tpgetName) `elem` ["timestamp", "time"]
      then arbitraryTimeZone
      else return ""

    tpgetDimensions <- arbitraryDimensions
    return $ TestPGExactType {..} where

      arbitraryTypeName = oneof [
          elements $
             [ "double precision"
             , "bit varying"
             , "character varying"
             , "timestamptz"
             , "timestamp"
             , "timetz"
             , "time"]
          ++ (map ("interval" ++) . ("":) . (map (' ':)) $ [
               "year to month"
             , "day to hour"
             , "day to minute"
             , "day to second"
             , "hour to minute"
             , "hour to second"
             , "minute to second"
             , "year"
             , "month"
             , "day"
             , "hour"
             , "minute"
             , "second"])
        , (render <$> (arbitrary :: Gen TestPGIdentifier))]

      arbitraryTypeModifier = oneof [
          render <$> (arbitrary :: Gen TestPGConstant)
        , render <$> (arbitrary :: Gen TestPGQualifiedIdentifier)]

      arbitraryTimeZone = elements ["with time zone", "without time zone", ""]

      arbitraryDimensions = oneof [
          return "array"
        , ("array " ++) <$> dimension
        , concat <$> listOf dimension] where
        dimension = oneof [
            show <$> (arbitrarySizedNatural :: Gen Int)
          , return ""
          ] >>= \d -> return . concat $ ["[", d, "]"]

  shrink (TestPGExactType n m t d) = [ TestPGExactType n m' t' d'
    | m' <- if null m then [] else [[], m]
    , t' <- shrinkString t
    , d' <- shrinkString d
    , (m /= m' || t /= t' || d /= d')] where
       shrinkString s = if (null s) then [s] else ["", s]

instance PGSql TestPGExactType where
  render (TestPGExactType {..})
    = intercalate " "
    . filter (not . null)
    $ [ tpgetName
      , if (not . null $ tpgetModifiers)
          then "(" ++ (intercalate "," tpgetModifiers) ++ ")"
          else ""
      , tpgetTimeZone
      , tpgetDimensions]


newtype TestPGType = TestPGType String deriving (Show, Eq)

instance Arbitrary TestPGType where
  arbitrary = TestPGType <$> oneof [
      render <$> (arbitrary :: Gen TestPGColumnType)
    , render <$> (arbitrary :: Gen TestPGExactType)]

instance PGSql TestPGType where
  render (TestPGType s) = s

instance PGTypeClass TestPGType where
  mergeTypes ts ts' =
    if ts == ts' || ((length ts' > 1) && ts == [recordType])
    then Just ts'
    else Nothing where
      recordType = TestPGType "record"



spec :: Spec
spec = do
  describe "pgColumnType" $ do
    propParsingWorks pgColumnType (Proxy :: Proxy TestPGColumnType)

  describe "pgExactType" $ do
    propParsingWorks pgExactType (Proxy :: Proxy TestPGExactType)

  describe "pgType" $ do
    propParsingWorks pgType (Proxy :: Proxy TestPGType)
