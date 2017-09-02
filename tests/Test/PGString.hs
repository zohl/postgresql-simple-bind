{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Test.PGString (
    TestPGTag(..)
  , TestPGQuotedString(..)
  , TestPGDollarQuotedString(..)
  , TestPGString(..)

  , spec
  ) where


import Data.Char (isNumber)
import Data.List (isInfixOf, isSuffixOf)
import Data.Proxy (Proxy(..))
import Data.Tagged (tagWith)
import Database.PostgreSQL.Simple.Bind.Parser
import Test.Hspec (Spec, shouldSatisfy, describe)
import Test.QuickCheck (Gen, Arbitrary(..), oneof, suchThat)
import qualified Data.Text as T
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, SomeSymbol(..), someSymbolVal)
import Test.Common (PGSql(..), proxyArbitrary, proxyMap, arbitraryString, arbitraryString', charASCII, charTag')
import Test.Utils (propParserRight, propParsingWorks)



newtype TestPGTag = TestPGTag String deriving (Show)

instance Arbitrary TestPGTag where
  arbitrary = TestPGTag <$> oneof [arbitraryString' charTag', return ""] where

instance PGSql TestPGTag where
  render (TestPGTag s) = s


newtype TestPGQuotedString (q :: Symbol) = TestPGQuotedString String deriving (Show)

instance (KnownSymbol q) => Arbitrary (TestPGQuotedString q) where
  arbitrary = TestPGQuotedString . concatMap doubleQuote <$> arbitraryString charASCII where
    doubleQuote c = if c == (head . symbolVal $ (Proxy :: Proxy q)) then [c, c] else [c]

instance (KnownSymbol q) => PGSql (TestPGQuotedString q) where
  render (TestPGQuotedString s) = s ++ (symbolVal (Proxy :: Proxy q))


newtype TestPGDollarQuotedString (tag :: Symbol) = TestPGDollarQuotedString String deriving (Show)

instance (KnownSymbol tag) => Arbitrary (TestPGDollarQuotedString tag) where
  arbitrary = let
    tagValue = render $ TestPGTag (symbolVal (Proxy :: Proxy tag))
    in TestPGDollarQuotedString <$> arbitraryString charASCII
      `suchThat` (not . isInfixOf ("$" ++ tagValue ++ "$"))
      `suchThat` (not . isSuffixOf "$")

instance (KnownSymbol tag) => PGSql (TestPGDollarQuotedString tag) where
  render (TestPGDollarQuotedString s) = let
    tagValue = render $ TestPGTag (symbolVal (Proxy :: Proxy tag))
    in concat [s, "$", tagValue, "$"]


newtype TestPGString = TestPGString String deriving (Show, Eq)

instance Arbitrary TestPGString where
  arbitrary = TestPGString <$> oneof [
      arbitraryQuotedString '"'
    , arbitraryQuotedString '\"'
    , (arbitrary :: Gen TestPGTag) >>= arbitraryDollarQuotedString . render
    ] where

    arbitraryQuotedString q = patternMatch (someSymbolVal [q]) where
      patternMatch (SomeSymbol x) = (q:) . render <$>
        (proxyArbitrary $ proxyMap (Proxy :: Proxy TestPGQuotedString) x)

    arbitraryDollarQuotedString tag = patternMatch (someSymbolVal tag) where
      patternMatch (SomeSymbol x) = (("$" ++ tag ++ "$") ++) . render <$>
        (proxyArbitrary $ proxyMap (Proxy :: Proxy TestPGDollarQuotedString) x)

instance PGSql TestPGString where
  render (TestPGString s) = s


spec :: Spec
spec = do
  describe "pgTag" $ do
    propParsingWorks pgTag (Proxy :: Proxy TestPGTag)
    let prop' = propParserRight (tagWith (Proxy :: Proxy TestPGTag) pgTag)
    prop' "the first symbol is not a number" . flip shouldSatisfy $ \s -> T.null s || (not . isNumber . T.head $ s)

  describe "pgQuotedString" $ do
    let prop' q = patternMatch (someSymbolVal [q]) where
          patternMatch (SomeSymbol x) = propParserRight
            (tagWith (proxyMap (Proxy :: Proxy TestPGQuotedString) x) (pgQuotedString q))
    let qs = ['"', '\'']
    let ps = [
            ("string is surrounded by quotes", \q -> flip shouldSatisfy $ \x -> T.head x == q && T.last x == q)
          , ("internal quotes are doubled", \q -> flip shouldSatisfy $
              null . filter ((/= 0) . (`mod` 2)) . map T.length
                   . filter ((== q) . T.head) . T.group
                   . T.tail . T.init)]
    mapM_
      (\(q, name, test) -> prop' q (name ++ "(" ++ [q] ++ ")") (test q))
      [(q, name, test) | q <- qs, (name, test) <- ps]

  describe "pgDollarQuotedString" $ do
    let prop' tag = patternMatch (someSymbolVal tag) where
          patternMatch (SomeSymbol x) = propParserRight
            (tagWith
               (proxyMap (Proxy :: Proxy TestPGDollarQuotedString) x)
               (pgDollarQuotedString $ T.pack ("$"++tag++"$")))
    let tags = ["", "foo", "bar_42"]
    let ps = [
            ("string ends with the tag", \tag -> flip shouldSatisfy $ T.isSuffixOf (T.pack ("$" ++ tag ++ "$")))]
    mapM_
      (\(tag, name, test) -> prop' tag (name ++ "(" ++ tag ++ ")") (test tag))
      [(tag, name, test) | tag <- tags, (name, test) <- ps]

  describe "pgString" $ do
    propParsingWorks pgString (Proxy :: Proxy TestPGString)
