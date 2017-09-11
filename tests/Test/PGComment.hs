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

module Test.PGComment (
    TestPGLineComment(..)
  , TestPGBlockComment(..)
  , TestPGComment(..)

  , spec
  ) where


import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Tagged (tagWith)
import Database.PostgreSQL.Simple.Bind.Parser
import Test.Hspec (Spec, describe, shouldSatisfy)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, suchThat)
import qualified Data.Text as T
import Test.Common (PGSql(..), arbitraryString, charASCII, charASCIInl, arbitrarySumDecomposition)
import Test.Utils (propParserRight, propParsingWorks)


newtype TestPGLineComment = TestPGLineComment String deriving (Show)

instance Arbitrary TestPGLineComment where
  arbitrary = TestPGLineComment <$> arbitraryString charASCII

instance PGSql TestPGLineComment where
  render (TestPGLineComment s) = "--" ++ s


data TestPGBlockComment
  = TestPGBlockCommentGroup [TestPGBlockComment]
  | TestPGBlockCommentElement String
  deriving (Show, Eq)

instance Arbitrary TestPGBlockComment where
  arbitrary = TestPGBlockCommentGroup <$> (sized $ \n -> arbitrarySumDecomposition n
    >>= mapM mkElement . zip (map ((== 0) . (`mod` 2)) [(1::Int)..])) where
      mkElement (isGroup, s) = resize s $ if isGroup
        then (arbitrary :: Gen TestPGBlockComment)
        else (TestPGBlockCommentElement <$> (arbitraryString charASCIInl)
                    `suchThat` (not . isInfixOf "/*")
                    `suchThat` (not . isInfixOf "*/")
                    `suchThat` (not . isSuffixOf "/")
                    `suchThat` (not . isSuffixOf "*"))


  shrink (TestPGBlockCommentGroup xs) = (concatMap shrink . filter isGroup $ xs)
                                     ++ (if (xs' /= xs)
                                         then [TestPGBlockCommentGroup xs']
                                         else []) where
    xs' = map (\x -> fromMaybe x . listToMaybe . shrink $ x) xs

    isGroup :: TestPGBlockComment -> Bool
    isGroup (TestPGBlockCommentGroup _) = True
    isGroup _                           = False

  shrink (TestPGBlockCommentElement s) = if length s > 3
    then [TestPGBlockCommentElement (head s:' ':last s:[])]
    else []

instance PGSql TestPGBlockComment where
  render (TestPGBlockCommentGroup xs) = "/*" ++ (concatMap render xs) ++ "*/"
  render (TestPGBlockCommentElement x) = x


newtype TestPGComment = TestPGComment String deriving (Show)

instance Arbitrary TestPGComment where
  arbitrary = TestPGComment <$> oneof [
      render <$> (arbitrary :: Gen TestPGLineComment)
    , render <$> (arbitrary :: Gen TestPGBlockComment)]

instance PGSql TestPGComment where
  render (TestPGComment s) = s


spec :: Spec
spec = do
  describe "pgLineComment" $ do
    propParsingWorks pgLineComment (Proxy :: Proxy TestPGLineComment)

  describe "pgBlockComment" $ do
    propParsingWorks pgBlockComment (Proxy :: Proxy TestPGBlockComment)

    let prop' = propParserRight (tagWith (Proxy :: Proxy TestPGBlockComment) pgBlockComment)
    prop' "starts with /*" . flip shouldSatisfy $ T.isPrefixOf "/*"
    prop' "ends with */" . flip shouldSatisfy $ T.isSuffixOf "*/"

  describe "pgComment" $ do
    propParsingWorks pgComment (Proxy :: Proxy TestPGComment)
