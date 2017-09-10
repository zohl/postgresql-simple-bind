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

import Test.Hspec (Spec, hspec)
import Data.Map.Strict (Map)
import Test.Utils (loadDirectory)

import qualified Test.PGString as PGString
import qualified Test.PGIdentifier as PGIdentifier
import qualified Test.PGComment as PGComment
import qualified Test.PGConstant as PGConstant
import qualified Test.PGType as PGType
import qualified Test.PGExpression as PGExpression
import qualified Test.PGResult as PGResult
import qualified Test.PGArgument as PGArgument
import qualified Test.PGFunction as PGFunction
import qualified Test.PGDeclaration as PGDeclaration


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
  PGDeclaration.spec' samples
