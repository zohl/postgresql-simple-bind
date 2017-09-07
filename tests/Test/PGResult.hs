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

module Test.PGResult (
    TestPGResult(..)
  , spec
  ) where

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Control.Monad (liftM2)
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGResultClass(..), PGTypeClass(..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary(..), oneof, suchThat, listOf1)
import Test.Common (PGSql(..))
import Test.Utils (propParsingWorks)
import Test.PGType (TestPGType(..))
import Test.PGIdentifier (TestPGIdentifier(..))


data TestPGResult
  = TestPGResultSingle [TestPGType]
  | TestPGResultSetOf [TestPGType]
  | TestPGResultTable [(TestPGIdentifier, TestPGType)]
    deriving (Show, Eq)

instance Arbitrary TestPGResult where
  arbitrary = oneof [arbitraryResultSingle, arbitraryResultSetOf, arbitraryResultTable] where
    arbitraryResultSingle = TestPGResultSingle . pure <$> (arbitrary `suchThat` (/= TestPGType "null"))
    arbitraryResultSetOf = TestPGResultSetOf . pure <$> arbitrary
    arbitraryResultTable = TestPGResultTable <$> listOf1 (liftM2 (,) arbitrary arbitrary)

instance PGSql TestPGResult where
  render (TestPGResultSingle s) = intercalate ", " . map render $ s
  render (TestPGResultSetOf s) = ("setof " ++) . (intercalate ", ") . map render $ s
  render (TestPGResultTable cs) =
    ("table (" ++) . (++ ")") . intercalate "," . map (\(c, t) -> render c ++ " " ++ render t) $ cs

instance PGResultClass TestPGResult TestPGType where
  mergeResults (TestPGResultSingle ts) (TestPGResultSingle ts') = TestPGResultSingle <$> (mergeTypes ts ts')
  mergeResults (TestPGResultSetOf  ts) (TestPGResultSetOf  ts') = TestPGResultSetOf  <$> (mergeTypes ts ts')
  mergeResults (TestPGResultSetOf  ts) (TestPGResultSingle ts') = TestPGResultSetOf  <$> (mergeTypes ts ts')
  mergeResults _                       _                        = Nothing

  resultSingle = TestPGResultSingle


spec :: Spec
spec = do
  describe "pgResult" $ do
    propParsingWorks pgResult (Proxy :: Proxy TestPGResult)
