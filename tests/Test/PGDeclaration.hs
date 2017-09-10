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

module Test.PGDeclaration (
    spec'
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGResult(..), PGIdentifier(..))
import Test.Hspec (Spec, describe, it)
import Data.Map.Strict (Map, (!))
import Test.Utils (testParser)

spec' :: Map FilePath Text -> Spec
spec' samples = do
  describe "pgDeclarations" $ do
    let test t = testParser pgDeclarations t . Right
    let functionTemplate = PGFunction {
          pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "" }
        , pgfArguments = []
        , pgfResult = PGSingle ["bigint"]}

    let foo = functionTemplate {pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }}
    let bar = functionTemplate {pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "bar" }}

    it "works with single function declaration" $ do
      test (samples ! "declarations-0001") [foo]

    it "works with multiple function declaration" $ do
      test (samples ! "declarations-0002") [foo, bar]

    it "ignores comments" $ do
      test (samples ! "declarations-0003") [foo, bar]

    it "ignores other clauses" $ do
      test (samples ! "declarations-0004") []
