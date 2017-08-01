{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Arrow (second)
import Data.Char (chr, isNumber, toLower)
import Data.List (isInfixOf, isSuffixOf, intercalate, tails)
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..), tagWith)
import Data.Either (isRight)
import Control.Monad (liftM2)
import Data.Attoparsec.Text (Parser, parseOnly, endOfInput)
import Data.Default (def)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResult(..), PGIdentifier(..), PGType(..))
import Text.Heredoc (str)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, choose, suchThat, frequency, arbitrarySizedNatural, listOf, listOf1, elements)
import qualified Data.Text as T
import qualified Data.Bifunctor as B(first)

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, SomeSymbol(..), someSymbolVal)

proxyArbitrary :: (Arbitrary a) => Proxy a -> Gen a
proxyArbitrary _ = arbitrary

proxyMap :: Proxy (f :: Symbol -> *) -> Proxy a -> Proxy (f a)
proxyMap _ _ = Proxy


class PGSql a where
  render :: a -> String

inClass :: String -> Gen Char
inClass = oneof . inClass' where
  inClass' (x:'-':y:xs) = (choose (x, y)):(inClass' xs)
  inClass' (x:xs)       = (return x):(inClass' xs)
  inClass' []           = []

arbitraryString :: Gen Char -> Gen String
arbitraryString c = sized $ \n -> sequence . replicate n $ c

arbitraryString' :: (Gen Char, Gen Char) -> Gen String
arbitraryString' (c, c') = sized $ \case
  0 -> return []
  1 -> pure <$> c
  n -> liftM2 (:) c (resize (n-1) $ arbitraryString c')

charASCII :: Gen Char
charASCII = inClass [chr 32, '-', chr 127]

charId :: (Gen Char, Gen Char)
charId = (inClass "A-Za-z_", inClass "A-Za-z0-9_$")

charTag :: (Gen Char, Gen Char)
charTag = (inClass "A-Za-z_", inClass "A-Za-z0-9_")

arbitrarySumDecomposition :: Int -> Gen [Int]
arbitrarySumDecomposition 0 = return []
arbitrarySumDecomposition n = choose (1, n) >>= \k -> (k:) <$> (arbitrarySumDecomposition (n-k))


data TestPGTag = TestPGTag String deriving (Show)

instance Arbitrary TestPGTag where
  arbitrary = TestPGTag <$> oneof [arbitraryString' charTag, return ""] where

instance PGSql TestPGTag where
  render (TestPGTag s) = s


data TestPGQuotedString (q :: Symbol) = TestPGQuotedString String deriving (Show)

instance (KnownSymbol q) => Arbitrary (TestPGQuotedString q) where
  arbitrary = TestPGQuotedString . concatMap doubleQuote <$> arbitraryString charASCII where
    doubleQuote c = if c == (head . symbolVal $ (Proxy :: Proxy q)) then [c, c] else [c]

instance (KnownSymbol q) => PGSql (TestPGQuotedString q) where
  render (TestPGQuotedString s) = s ++ (symbolVal (Proxy :: Proxy q))


data TestPGDollarQuotedString (tag :: Symbol) = TestPGDollarQuotedString String deriving (Show)

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


data TestPGString = TestPGString String deriving (Show)

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


data TestPGNormalIdentifier = TestPGNormalIdentifier String deriving (Show)

instance Arbitrary TestPGNormalIdentifier where
  arbitrary = TestPGNormalIdentifier <$> (arbitraryString' charId) `suchThat` ((> 0) . length)

instance PGSql TestPGNormalIdentifier where
  render (TestPGNormalIdentifier s) = s


data TestPGIdentifier = TestPGIdentifier String deriving (Show)

instance Arbitrary TestPGIdentifier where
  arbitrary = TestPGIdentifier <$> oneof [
      render <$> (arbitrary :: Gen TestPGNormalIdentifier)
    , ('"':) . render <$> (arbitrary :: Gen (TestPGQuotedString "\""))]

instance PGSql TestPGIdentifier where
  render (TestPGIdentifier s) = s


data TestPGQualifiedIdentifier = TestPGQualifiedIdentifier PGIdentifier deriving (Show)

instance Arbitrary TestPGQualifiedIdentifier where
  arbitrary = do
    pgiSchema <- oneof [return Nothing, Just . render <$> (arbitrary :: Gen TestPGIdentifier)]
    pgiName   <- render <$> (arbitrary :: Gen TestPGIdentifier)
    return $ TestPGQualifiedIdentifier PGIdentifier {..}

instance PGSql TestPGQualifiedIdentifier where
  render (TestPGQualifiedIdentifier PGIdentifier {..}) = maybe pgiName (++ ('.':pgiName)) pgiSchema


data TestPGLineComment = TestPGLineComment String deriving (Show)

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
        else (TestPGBlockCommentElement <$> (arbitraryString $ frequency [(15, charASCII), (1, return '\n')])
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


data TestPGComment = TestPGComment String deriving (Show)

instance Arbitrary TestPGComment where
  arbitrary = TestPGComment <$> oneof [
      render <$> (arbitrary :: Gen TestPGLineComment)
    , render <$> (arbitrary :: Gen TestPGBlockComment)]

instance PGSql TestPGComment where
  render (TestPGComment s) = s


data TestPGColumnType = TestPGColumnType String deriving (Show)

instance Arbitrary TestPGColumnType where
  arbitrary = TestPGColumnType <$> do
    tableName  <- render <$> (arbitrary :: Gen TestPGIdentifier)
    columnName <- render <$> (arbitrary :: Gen TestPGIdentifier)
    return $ concat [tableName, ".", columnName, "%type"]

instance PGSql TestPGColumnType where
  render (TestPGColumnType s) = s


data TestPGExactType = TestPGExactType String String String String deriving (Show)

instance Arbitrary TestPGExactType where
  arbitrary = do
    name       <- arbitraryTypeName
    modifier   <- arbitraryTypeModifier
    timeZone   <- if (map toLower name) `elem` ["timestamp", "time"]
                    then arbitraryTimeZone
                    else return ""
    dimensions <- arbitraryDimensions
    return $ TestPGExactType name modifier timeZone dimensions where

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
        , (render <$> (arbitrary :: Gen TestPGIdentifier)) `suchThat` (not . null)]

      arbitraryTypeModifier = oneof [
            return ""
          , (render <$> (arbitrary :: Gen TestPGString))
              `suchThat` (not . null)
              `suchThat` (/= "\"\"")
              >>= \s -> return . concat $ ["(", s, ")"]
          , (arbitraryString charASCII)
              `suchThat` (not . elem '(')
              `suchThat` (not . elem ')')
              `suchThat` (not . elem '\'')
              `suchThat` (not . elem '"')
              >>= \s -> return . concat $ ["(", s, ")"]]

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
    | m' <- shrinkString m
    , t' <- shrinkString t
    , d' <- shrinkString d
    , (m /= m' || t /= t' || d /= d')] where
       shrinkString s = if (null s) then [s] else ["", s]

instance PGSql TestPGExactType where
  render (TestPGExactType name modifier timeZone dimensions) = intercalate " " . filter (not . null) $
    [name, modifier, timeZone, dimensions]


data TestPGType = TestPGType String deriving (Show)

instance Arbitrary TestPGType where
  arbitrary = TestPGType <$> oneof [
      render <$> (arbitrary :: Gen TestPGColumnType)
    , render <$> (arbitrary :: Gen TestPGExactType)]

instance PGSql TestPGType where
  render (TestPGType s) = s


data TestPGResult
  = TestPGResultSingle String
  | TestPGResultSetOf String
  | TestPGResultTable [(String, String)]
    deriving (Show)

instance Arbitrary TestPGResult where
  arbitrary = oneof [arbitraryResultSingle, arbitraryResultSetOf, arbitraryResultTable] where
    arbitraryResultSingle = TestPGResultSingle . render <$> (arbitrary :: Gen TestPGType)
    arbitraryResultSetOf = TestPGResultSetOf . render <$> (arbitrary :: Gen TestPGType)
    arbitraryResultTable = TestPGResultTable <$> listOf1 (liftM2 (,)
      (render <$> (arbitrary :: Gen TestPGIdentifier))
      (render <$> (arbitrary :: Gen TestPGType)))

instance PGSql TestPGResult where
  render (TestPGResultSingle s) = s
  render (TestPGResultSetOf s) = "setof " ++ s
  render (TestPGResultTable cs) = "table (" ++ (intercalate ", " . map (uncurry (++) . second (' ':)) $ cs) ++ ")"


instance Arbitrary PGArgumentMode where
  arbitrary = elements [In, Out, InOut, Variadic]

data TestPGArgument = TestPGArgument {
    tpgaMode            :: Maybe PGArgumentMode
  , tpgaName            :: Maybe TestPGIdentifier
  , tpgaType            :: TestPGType
  , tpgaDefaultNotation :: String
  , tpgaDefaultValue    :: Maybe String
  } deriving (Show)

instance Arbitrary TestPGArgument where
  arbitrary = do
    tpgaMode            <- arbitrary
    tpgaName            <- arbitrary
    tpgaType            <- arbitrary
    tpgaDefaultNotation <- elements ["=", "default"]
    tpgaDefaultValue    <- elements [Nothing, Just "expression"] -- TODO
    return TestPGArgument {..}

instance PGSql TestPGArgument where
  render (TestPGArgument {..}) = concat . catMaybes $ [
      ((++ " ") . show) <$> tpgaMode
    , (++ " ") . render <$> tpgaName
    , Just . render $ tpgaType
    , (' ':)  . (tpgaDefaultNotation ++) . (' ':) <$> tpgaDefaultValue]


data TestPGArgumentList = TestPGArgumentList [TestPGArgument] deriving (Show)

instance Arbitrary TestPGArgumentList where
  arbitrary = TestPGArgumentList <$> (listOf arbitrary)
    `suchThat` (wrap checkExpectedDefaults)
    `suchThat` (wrap checkNotExpectedDefaults)
    `suchThat` (wrap checkVariadic) where
    wrap check
      = maybe True (const False)
      . check
          (fromMaybe In . tpgaMode)
          (maybe False (const True) . tpgaDefaultValue)

  shrink (TestPGArgumentList xs) = map TestPGArgumentList (tail . tails $ xs)

instance PGSql TestPGArgumentList where
  render (TestPGArgumentList xs) = intercalate ", " . map render $ xs


propParser :: forall a b. (PGSql a, Arbitrary a, Show a, Show b)
  => Tagged a (Parser b)
  -> String
  -> (b -> Expectation)
  -> Spec
propParser p name t = prop name property where

  property :: a -> Expectation
  property x = test (parseOnly (unTagged p <* endOfInput) (T.pack . render $ x))

  test :: Either String b -> Expectation
  test result = either
    (const $ result `shouldSatisfy` isRight)
    t
    result

main :: IO ()
main = hspec spec

testParser :: (Show a, Eq a) => Parser a -> Text -> Either ParserException a -> IO ()
testParser parser text result =
  (parseOnly (parser <* endOfInput) text)
  `shouldBe`
  (B.first ((prefix ++) . show) result) where
    prefix = either id (const "") $ parseOnly (fail "") ""


spec :: Spec
spec = do
  describe "pgTag" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGTag) pgTag)
    prop' "the first symbol is not a number" . flip shouldSatisfy $ \s -> T.null s || (not . isNumber . T.head $ s)

  describe "pgQuotedString" $ do
    let prop' q = patternMatch (someSymbolVal [q]) where
          patternMatch (SomeSymbol x) = propParser
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
          patternMatch (SomeSymbol x) = propParser
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
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGString) pgString)
    prop' "parsing works" . flip shouldSatisfy $ const True

  describe "pgNormalIdentifier" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGNormalIdentifier) pgNormalIdentifier)
    prop' "the first symbol is not '$'" . flip shouldSatisfy $ \s -> (T.head s) /= '$'
    prop' "stored in lowercase" $ \x -> x `shouldBe` (T.toLower x)

  describe "pgIdentifier" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGIdentifier) pgIdentifier)
    prop' "parsing works" $ flip shouldSatisfy $ const True

  describe "pgQualifiedIdentifier" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGQualifiedIdentifier) pgQualifiedIdentifier)
    prop' "parsing works" $ flip shouldSatisfy $ const True

  describe "pgLineComment" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGLineComment) pgLineComment)
    prop' "starts with \"--\"" . flip shouldSatisfy $ T.isPrefixOf "--"

  describe "pgBlockComment" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGBlockComment) pgBlockComment)
    prop' "starts with /*" . flip shouldSatisfy $ T.isPrefixOf "/*"
    prop' "ends with */" . flip shouldSatisfy $ T.isSuffixOf "*/"

  describe "pgComment" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGComment) pgComment)
    prop' "parsing works" . flip shouldSatisfy $ const True

  describe "pgColumnType" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGColumnType) pgColumnType)
    prop' "parsing works" . flip shouldSatisfy $ const True

  describe "pgExactType" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGExactType) pgExactType)
    prop' "parsing works" . flip shouldSatisfy $ const True

  describe "pgType" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGType) pgType)
    prop' "parsing works" . flip shouldSatisfy $ const True

  describe "pgResult" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGResult) pgResult)
    prop' "parsing works" . flip shouldSatisfy $ const True

  describe "pgArgument" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGArgument) pgArgument)
    prop' "parsing works" . flip shouldSatisfy $ const True

  describe "pgArgumentList" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGArgumentList) (pgArgumentList True))
    prop' "parsing works" . flip shouldSatisfy $ const True


  describe "pgArgumentList" $ do
    let test t = testParser (pgArgumentList True) t . Right
    it "works with mixed arguments" $ do
      test "in p1 bigint default 'test', out p2 varchar" [
          PGArgument { pgaMode = In,  pgaName = Just "p1", pgaType = "bigint",  pgaOptional = True }
        , PGArgument { pgaMode = Out, pgaName = Just "p2", pgaType = "varchar", pgaOptional = False }]


  describe "pgArgumentList (incorrect declarations)" $ do
    let test t = testParser (pgArgumentList True) t . Left

    it "fails when an optional argument is followed by mandatory one" $ do
      test "p1 bigint, p2 varchar default 'foo', p3 varchar"
        (DefaultValueExpected
          PGArgument { pgaMode = In, pgaName = Just "p3", pgaType = "varchar", pgaOptional = False })

    it "fails when VARIADIC variable followed by non-OUT variable" $ do
      test "variadic p1 bigint, out p2 varchar, in p3 varchar"
        (NonOutVariableAfterVariadic
          PGArgument { pgaMode = In, pgaName = Just "p3", pgaType = "varchar", pgaOptional = False })

    it "fails when OUT or VARIADIC variable specified with default value" $ do
      test "out p bigint default 1"
        (DefaultValueNotExpected
          PGArgument { pgaMode = Out, pgaName = Just "p", pgaType = "bigint", pgaOptional = True })
      test "variadic p bigint default 1"
        (DefaultValueNotExpected
          PGArgument { pgaMode = Variadic, pgaName = Just "p", pgaType = "bigint", pgaOptional = True })




  describe "pgFunction" $ do
    let test t = testParser pgFunction t . Right

    it "works with simple declarations" $ do
      test
        [str|create function foo()
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult = PGSingle ["bigint"]
          }

      test
        [str|create or replace function foo()
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult = PGSingle ["bigint"]
          }

      test
        [str|create function foo(p_bar varchar)
            |returns bigint as
            |$$ select 42::bigint $$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = [
                PGArgument {pgaMode = def, pgaName = Just "p_bar", pgaType = "varchar", pgaOptional = False}]
          , pgfResult = PGSingle ["bigint"]
          }

      test
        [str|create function foo(p_bar varchar, p_baz varchar)
            |returns bigint as
            |$body$
            |  select 42::bigint'
            |$body$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = [
                PGArgument {pgaMode = def, pgaName = Just "p_bar", pgaType = "varchar", pgaOptional = False}
              , PGArgument {pgaMode = def, pgaName = Just "p_baz", pgaType = "varchar", pgaOptional = False}]
          , pgfResult = PGSingle ["bigint"]
          }

    it "works with schema-qualified functions" $ do
      test
        [str|create function public.foo()
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Just "public", pgiName = "foo" }
          , pgfArguments = []
          , pgfResult = PGSingle ["bigint"]
          }

    it "works with single OUT parameter" $ do
      test
        [str|create function foo(out p_result bigint) as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["bigint"]
          }

      test
        [str|create function foo(out p_result bigint)
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["bigint"]
          }

    it "works with multiple OUT parameters" $ do
      test
        [str|create function foo(out p1 bigint, out p2 varchar) as
            |$$ select 42::bigint, 'test'::varchar $$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["bigint", "varchar"]
          }

      test
        [str|create function foo(out p1 bigint, out p2 varchar)
            |returns record as
            |$$ select 42::bigint, 'test'::varchar $$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["bigint", "varchar"]
          }

    it "works with OUT parameters and SETOF notation" $ do
      test
        [str|create function foo(out p_result bigint)
            |returns setof bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSetOf ["bigint"]
          }

      test
        [str|create function foo(out p1 bigint, out p2 varchar)
            |returns setof record as
            |$$ select 42::bigint, 'test'::varchar $$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSetOf ["bigint", "varchar"]
          }

    it "works with different properties" $ do
      let test' s = test s PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["void"]
          }

      test' [str|create function foo() returns void as '' language 'sql'|]
      test' [str|create function foo() returns void as '' language plpgsql|]

      test' [str|create function foo() returns void as 'test.o', 'foo' language C|]

      test' [str|create function foo() returns void as '' window|]

      test' [str|create function foo() returns void as '' immutable|]
      test' [str|create function foo() returns void as '' stable|]
      test' [str|create function foo() returns void as '' volatile|]

      test' [str|create function foo() returns void as '' leakproof|]
      test' [str|create function foo() returns void as '' not leakproof|]

      test' [str|create function foo() returns void as '' called on null input|]
      test' [str|create function foo() returns void as '' returns null on null input|]
      test' [str|create function foo() returns void as '' strict|]

      test' [str|create function foo() returns void as '' external security invoker|]
      test' [str|create function foo() returns void as '' security definer|]

      test' [str|create function foo() returns void as '' parallel unsafe|]
      test' [str|create function foo() returns void as '' parallel restricted|]
      test' [str|create function foo() returns void as '' parallel safe|]

      test' [str|create function foo() returns void as '' cost 100|]

      test' [str|create function foo() returns void as '' rows 100|]

      test' [str|create function foo() returns void as '' with isStrict|]
      test' [str|create function foo() returns void as '' with isCachable, isStrict|]

  describe "pgFunction (incorrect declarations)" $ do
    let test t = testParser pgFunction t . Left

    it "fails when cannot determine return type" $ do
      test
        "create function foo() as 'select 42::bigint'"
        NoReturnTypeInfo

    it "fails when return types are incoherent" $ do
      test
        [str|create function foo(out p1 bigint, out p2 varchar)
            |returns timestamptz as
            |$$ select 42::bigint, 'test'::varchar $$|]
        (IncoherentReturnTypes
          (PGSingle ["timestamptz"])
          (PGSingle ["bigint", "varchar"]))

      test
        [str|create function foo(out p1 bigint)
            |returns table (p1 bigint) as ''|]
        (IncoherentReturnTypes
          (PGTable [PGColumn {pgcName = "p1", pgcType = "bigint"}])
          (PGSingle ["bigint"]))


  describe "pgDeclarations" $ do
    let test t = testParser pgDeclarations t . Right
    let f = PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult = PGSingle ["bigint"]
          }

    it "works with single function declaration" $ do
      test
        "create function foo() returns bigint as 'select 42::bigint';"
        [f]

    it "works with multiple function declaration" $ do
      test
        [str|create function foo() returns bigint as 'select 42::bigint';
            |create function bar() returns bigint as 'select 42::bigint';
            |]
        [f, f {pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "bar" }}]

    it "ignores comments" $ do
      test
        [str| -- foo
            |create function foo() returns bigint as 'select 42::bigint';
            |
            | /* bar
            |  *
            |  */
            |create function bar() returns bigint as 'select 42::bigint';
            |]
        [f, f {pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "bar" }}]

    it "ignores other clauses" $ do
      test
        [str|create table t (f_id bigint, f_body varchar);
            |insert into t (f_id, f_body) values (1, 'create function foo() bigint as $$ select 1; $$;');
            |insert into t (f_id, f_body) values (2, 'create function bar() bigint as $$ select 2; $$;');
            |commit;
            |
            |perform 'create function baz() returns void as $$ select 3; $$';
            |perform '; create function qux() returns void as $$ select 3; $$; ';
            |
            |]
        []
