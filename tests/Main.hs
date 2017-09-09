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

module Main where

import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGResult(..), PGIdentifier(..))
import Test.Hspec (Spec, hspec, describe, it)
import Data.Map.Strict (Map, (!))
import Test.Utils (testParser, loadDirectory)

import qualified Test.PGString as PGString
import qualified Test.PGIdentifier as PGIdentifier
import qualified Test.PGComment as PGComment
import qualified Test.PGConstant as PGConstant
import qualified Test.PGType as PGType
import qualified Test.PGExpression as PGExpression
import qualified Test.PGResult as PGResult
import qualified Test.PGArgument as PGArgument
import qualified Test.PGFunction as PGFunction


main :: IO ()
main = do
  samples <- loadDirectory "tests/samples"
  hspec (spec samples)


spec :: Map FilePath Text -> Spec
spec samples = do
  PGString.spec
  PGIdentifier.spec
  PGComment.spec
  PGConstant.spec
  PGType.spec
  PGExpression.spec
  PGResult.spec
  PGArgument.spec
  PGFunction.spec

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
