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

import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.Either (isRight)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgumentClass(..), PGArgumentMode(..), PGResult(..), PGIdentifier(..))
import Test.Hspec (Spec, hspec, describe, it)
import Test.QuickCheck (Gen, Arbitrary(..), resize, oneof, suchThat, arbitrarySizedNatural, listOf, listOf1, elements)
import Data.Map.Strict (Map, (!))
import Test.Common (PGSql(..))
import Test.Utils (testParser, loadDirectory)

import Test.PGString (TestPGQuotedString(..), TestPGString(..))
import qualified Test.PGString as PGString

import Test.PGIdentifier (TestPGNormalIdentifier(..), TestPGQualifiedIdentifier(..))
import qualified Test.PGIdentifier as PGIdentifier

import qualified Test.PGComment as PGComment

import qualified Test.PGConstant as PGConstant

import Test.PGType (TestPGExactType(..))
import qualified Test.PGType as PGType


import qualified Test.PGExpression as PGExpression

import Test.PGResult (TestPGResult(..))
import qualified Test.PGResult as PGResult

import Test.PGArgument (TestPGArgumentList(..), TPGALCorrect(..))
import qualified Test.PGArgument as PGArgument


data TestPGFunction = TestPGFunction {
    tpgfIdentifier         :: TestPGQualifiedIdentifier
  , tpgfOrReplace          :: Bool
  , tpgfArgumentList       :: TestPGArgumentList
  , tpgfResult             :: Maybe TestPGResult
  , tpgfProperties         :: [String]
  , tpgfObsoleteProperties :: [String]
  } deriving (Show)

instance Arbitrary TestPGFunction where
  arbitrary = do
    tpgfOrReplace          <- arbitrary
    tpgfIdentifier         <- arbitrary
    tpgfArgumentList       <- getArgumentList <$> arbitrary
    tpgfResult             <- arbitrary `suchThat` (isJust)
    tpgfProperties         <- listOf property
    tpgfObsoleteProperties <- resize 2 $ listOf obsoleteProperty

    return TestPGFunction {..} where
      property = oneof [
          language
        , loadableObject
        , definition
        , window
        , behaviour
        , leakproof
        , strictness
        , security
        , parallel
        , cost
        , rows
        , transform
        , set]

      language = ("language " ++) <$> oneof [
          render <$> (arbitrary :: Gen TestPGNormalIdentifier)
        , ('\'':) . render <$> (arbitrary :: Gen (TestPGQuotedString "'"))]

      loadableObject = do
        s1 <- render <$> (arbitrary :: Gen TestPGString)
        s2 <- render <$> (arbitrary :: Gen TestPGString)
        return . concat $ ["as ", s1, ", ", s2]

      definition = ("as " ++) . render <$> (arbitrary :: Gen TestPGString)

      window = return "window"

      behaviour = elements ["immutable", "stable", "volatile"]

      leakproof = do
        not' <- elements ["", "not "]
        return . concat $ [not', "leakproof"]

      strictness = elements [
          "called on null input"
        , "returns null on null input"
        , "strict"]

      security = do
        external <- elements ["", "external "]
        authority <- elements ["invoker", "definer"]
        return . concat $ [external, "security ", authority]

      parallel = ("parallel " ++) <$> elements ["unsafe", "restricted", "safe"]

      cost = ("cost " ++) . show <$> (arbitrarySizedNatural :: Gen Int)

      rows = ("rows " ++) . show <$> (arbitrarySizedNatural :: Gen Int)

      transform = do
        types <- listOf1 (arbitrary :: Gen TestPGExactType)
        return $ "transform " ++ (intercalate "," . map (("for type " ++) . render) $ types)

      set = do
        identifier <- render <$> (arbitrary :: Gen TestPGNormalIdentifier)

        let value = (intercalate ", ") <$> (resize 4 . listOf1 . oneof $ [
                render <$> (arbitrary :: Gen TestPGString)
              , show <$> (arbitrarySizedNatural :: Gen Int)
              , render <$> (arbitrary :: Gen TestPGQualifiedIdentifier)])

        assignment <- oneof [
            (" to " ++) <$> value
          , (" = " ++) <$> value
          , return " from current"]

        return . concat $ ["set ", identifier, assignment]


      obsoleteProperty = elements ["isStrict", "isCachable"]

  shrink f@(TestPGFunction {..}) = if (not . null $ tpgfProperties)
    then [f {tpgfProperties = ps'} | ps' <- [filter (/= p) tpgfProperties | p <- tpgfProperties]]
    else []

instance PGSql TestPGFunction where
  render (TestPGFunction {..}) = concat $ [
      "create "
    , if tpgfOrReplace then "or replace " else ""
    , "function "
    , render tpgfIdentifier
    , " (", render tpgfArgumentList, ") "
    , fromMaybe "" . fmap (("returns " ++)  . render) $ tpgfResult
    , if (not . null $ tpgfProperties) then " " else ""
    , intercalate " " tpgfProperties
    , if (not . null $ tpgfObsoleteProperties) then " with " else ""
    , intercalate "," tpgfObsoleteProperties]


newtype TPGFCorrect = TPGFCorrect TestPGFunction deriving (Show)

instance Arbitrary TPGFCorrect where
  arbitrary = TPGFCorrect <$> arbitrary
    `suchThat` (\TestPGFunction {..} -> isRight . normalizeFunction $ (getArguments tpgfArgumentList, tpgfResult))

instance PGSql TPGFCorrect where
  render (TPGFCorrect x) = render x


newtype TPGFFailedNoReturnTypeInfo = TPGFFailedNoReturnTypeInfo TestPGFunction deriving (Show)

instance Arbitrary TPGFFailedNoReturnTypeInfo where
  arbitrary = TPGFFailedNoReturnTypeInfo <$> arbitraryFunction
    `suchThat` (\TestPGFunction {..} -> either match (const False)
                 . normalizeFunction $ (getArguments tpgfArgumentList, tpgfResult))
    where
      arbitraryFunction = do
        f <- arbitrary
        let args = filter ((`elem` [In, InOut]) . argumentMode) . getArguments . tpgfArgumentList $ f

        return f {
            tpgfResult = Nothing
          , tpgfArgumentList = TestPGArgumentList args
          }

      match :: ParserException -> Bool
      match NoReturnTypeInfo = True
      match _                = False

instance PGSql TPGFFailedNoReturnTypeInfo where
  render (TPGFFailedNoReturnTypeInfo x) = render x


newtype TPGFFailedIncoherentReturnTypes = TPGFFailedIncoherentReturnTypes TestPGFunction deriving (Show)

instance Arbitrary TPGFFailedIncoherentReturnTypes where
  arbitrary = TPGFFailedIncoherentReturnTypes <$> arbitrary
    `suchThat` (\TestPGFunction {..} -> either match (const False)
                 . normalizeFunction $ (getArguments tpgfArgumentList, tpgfResult))
    where
      match :: ParserException -> Bool
      match (IncoherentReturnTypes _ _) = True
      match _                           = False

instance PGSql TPGFFailedIncoherentReturnTypes where
  render (TPGFFailedIncoherentReturnTypes x) = render x


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

--  describe "pgFunction" $ do
--    propParsingWorks pgFunction (Proxy :: Proxy TPGFCorrect)
--    propParsingFails pgFunction "NoReturnTypeInfo" (Proxy :: Proxy TPGFFailedNoReturnTypeInfo)
--    propParsingFails pgFunction "IncoherentReturnTypes" (Proxy :: Proxy TPGFFailedIncoherentReturnTypes)

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
