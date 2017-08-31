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

module Test.Utils (
    propParser
  , propParserLeft
  , propParserRight
  , propParsingWorks
  , propParsingFails

  , testParser

  , loadDirectory
  ) where


import Data.List (isPrefixOf)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..), tagWith)
import Data.Attoparsec.Text (Parser, parseOnly, endOfInput)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Parser
import Test.Hspec (Expectation, Spec, shouldSatisfy, shouldBe, expectationFailure)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Text as T
import qualified Data.Bifunctor as B(first)
import System.Directory (listDirectory)
import System.FilePath.Posix (dropExtensions, (</>), (<.>))
import qualified Data.Text.IO as T
import Test.Common (PGSql(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


propParser :: forall a b. (PGSql a, Arbitrary a, Show a, Show b)
  => Tagged a (Parser b)
  -> String
  -> (a -> Either String b -> Expectation)
  -> Spec
propParser p name test = prop name property where
  property :: a -> Expectation
  property x = test x (parseOnly (unTagged p <* endOfInput) (T.pack . render $ x))

propParserLeft :: forall a b. (PGSql a, Arbitrary a, Show a, Show b)
  => Tagged a (Parser b)
  -> String
  -> (String -> Expectation)
  -> Spec
propParserLeft p name test = propParser p name $ \x r -> either
  (test . drop (length prefix))
  (const . expectationFailure $ "expected parser failure at the following string:\n" ++ render x)
  r where
    prefix = either id (const "") $ parseOnly (fail "") ""

propParserRight :: forall a b. (PGSql a, Arbitrary a, Show a, Show b)
  => Tagged a (Parser b)
  -> String
  -> (b -> Expectation)
  -> Spec
propParserRight p name t = propParser p name $ \x r -> either
  (\err -> expectationFailure $ "unexpected parser failure (" ++ err ++ ") at the following string:\n" ++ render x)
  t
  r

propParsingWorks :: forall a b. (PGSql a, Arbitrary a, Show a, Show b)
  => (Parser b)
  -> Proxy a
  -> Spec
propParsingWorks p t = propParserRight (tagWith t p) "parsing works" (flip shouldSatisfy $ const True)

propParsingFails :: forall a b. (PGSql a, Arbitrary a, Show a, Show b)
  => (Parser b)
  -> String
  -> Proxy a
  -> Spec
propParsingFails p e t =  propParserLeft (tagWith t p) ("throws " ++ e) (flip shouldSatisfy $ isPrefixOf e)


loadFile :: FilePath -> FilePath -> IO Text
loadFile dir fn = T.readFile (dir </> fn <.> "sql")

loadDirectory :: FilePath -> IO (Map FilePath Text)
loadDirectory dir = listDirectory dir
                >>= fmap Map.fromList . mapM (\fn -> (fn,) <$> (loadFile dir fn)) . map dropExtensions


testParser :: (Show a, Eq a) => Parser a -> Text -> Either ParserException a -> IO ()
testParser parser text result =
  (parseOnly (parser <* endOfInput) text)
  `shouldBe`
  (B.first ((prefix ++) . show) result) where
    prefix = either id (const "") $ parseOnly (fail "") ""
