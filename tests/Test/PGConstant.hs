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

module Test.PGConstant (
    TestPGConstant(..)

  , spec
  ) where

import Data.Proxy (Proxy(..))
import Database.PostgreSQL.Simple.Bind.Parser
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary(..), oneof)
import Test.Common (PGSql(..))
import Test.Utils (propParsingWorks)
import Test.PGString (TestPGString(..))


data TestPGConstant
  = TPGCString TestPGString
  | TPGCNumeric Double
  deriving (Show, Eq)

instance Arbitrary TestPGConstant where
  arbitrary = oneof [
      TPGCString <$> arbitrary
    , TPGCNumeric <$> arbitrary]

instance PGSql TestPGConstant where
  render (TPGCString s) = render s
  render (TPGCNumeric c) = show c


spec :: Spec
spec = do
  describe "pgConstant" $ do
    propParsingWorks pgConstant (Proxy :: Proxy TestPGConstant)
