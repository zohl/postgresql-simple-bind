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

import Data.Char (toLower)
import Data.List (isInfixOf, intercalate, tails)
import Data.Maybe (fromMaybe, catMaybes, isJust, isNothing)
import Data.Proxy (Proxy(..))

import Data.Either (isRight)
import Control.Monad (liftM2)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgumentClass(..), PGArgumentMode(..), PGResultClass(..), PGResult(..), PGIdentifier(..), PGTypeClass(..))
import Test.Hspec (Spec, hspec, describe, it)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, suchThat, arbitrarySizedNatural, listOf, listOf1, elements, arbitraryBoundedEnum)

import Data.Map.Strict (Map, (!))
import Test.Common (PGSql(..), arbitraryString, charOperator)
import Test.Utils (propParsingWorks, propParsingFails, testParser, loadDirectory)

import Test.PGString (TestPGQuotedString(..), TestPGString(..))
import qualified Test.PGString as PGString

import Test.PGIdentifier (TestPGNormalIdentifier(..), TestPGQualifiedIdentifier(..), TestPGIdentifier(..))
import qualified Test.PGIdentifier as PGIdentifier

import qualified Test.PGComment as PGComment


newtype TestPGColumnType = TestPGColumnType String deriving (Show)

instance Arbitrary TestPGColumnType where
  arbitrary = TestPGColumnType <$> do
    tableName  <- render <$> (arbitrary :: Gen TestPGIdentifier)
    columnName <- render <$> (arbitrary :: Gen TestPGIdentifier)
    return $ concat [tableName, ".", columnName, "%type"]

instance PGSql TestPGColumnType where
  render (TestPGColumnType s) = s


data TestPGExactType = TestPGExactType {
    tpgetName       :: String
  , tpgetModifiers  :: [String]
  , tpgetTimeZone   :: String
  , tpgetDimensions :: String
  } deriving (Show)

instance Arbitrary TestPGExactType where
  arbitrary = do
    tpgetName       <- arbitraryTypeName
    tpgetModifiers  <- listOf arbitraryTypeModifier
    tpgetTimeZone   <- if (map toLower tpgetName) `elem` ["timestamp", "time"]
      then arbitraryTimeZone
      else return ""

    tpgetDimensions <- arbitraryDimensions
    return $ TestPGExactType {..} where

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
        , (render <$> (arbitrary :: Gen TestPGIdentifier))]

      arbitraryTypeModifier = oneof [
          render <$> (arbitrary :: Gen TestPGConstant)
        , render <$> (arbitrary :: Gen TestPGQualifiedIdentifier)]

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
    | m' <- if null m then [] else [[], m]
    , t' <- shrinkString t
    , d' <- shrinkString d
    , (m /= m' || t /= t' || d /= d')] where
       shrinkString s = if (null s) then [s] else ["", s]

instance PGSql TestPGExactType where
  render (TestPGExactType {..})
    = intercalate " "
    . filter (not . null)
    $ [ tpgetName
      , if (not . null $ tpgetModifiers)
          then "(" ++ (intercalate "," tpgetModifiers) ++ ")"
          else ""
      , tpgetTimeZone
      , tpgetDimensions]


newtype TestPGType = TestPGType String deriving (Show, Eq)

instance Arbitrary TestPGType where
  arbitrary = TestPGType <$> oneof [
      render <$> (arbitrary :: Gen TestPGColumnType)
    , render <$> (arbitrary :: Gen TestPGExactType)]

instance PGSql TestPGType where
  render (TestPGType s) = s

instance PGTypeClass TestPGType where
  mergeTypes ts ts' =
    if ts == ts' || ((length ts' > 1) && ts == [recordType])
    then Just ts'
    else Nothing where
      recordType = TestPGType "record"


newtype TestPGOperator = TestPGOperator String deriving (Show)

instance Arbitrary TestPGOperator where
  arbitrary = TestPGOperator <$> arbitraryString charOperator `suchThat` (not . null)
    `suchThat` (not . isInfixOf "--")
    `suchThat` (not . isInfixOf "/*")
    `suchThat` (\s -> (not . (`elem` ("+-"::String)) . last $ s)
                   || (not . null . filter (`elem` ("~!@#%^&|`?"::String)) $ s))
instance PGSql TestPGOperator where
  render (TestPGOperator s) = s


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


instance Arbitrary PGArgumentMode where
  arbitrary = arbitraryBoundedEnum

data TestPGArgument = TestPGArgument {
    tpgaMode            :: Maybe PGArgumentMode
  , tpgaName            :: Maybe TestPGIdentifier
  , tpgaType            :: TestPGType
  , tpgaDefaultNotation :: String
  , tpgaDefaultValue    :: Maybe TestPGExpression
  } deriving (Show, Eq)

instance Arbitrary TestPGArgument where
  arbitrary = do
    tpgaMode            <- arbitrary
    tpgaName            <- arbitrary
    tpgaType            <- arbitrary
    tpgaDefaultNotation <- elements ["=", "default"]
    tpgaDefaultValue    <- arbitrary
    return TestPGArgument {..}

instance PGSql TestPGArgument where
  render (TestPGArgument {..}) = concat . catMaybes $ [
      ((++ " ") . show) <$> tpgaMode
    , (++ " ") . render <$> tpgaName
    , Just . render $ tpgaType
    , (' ':)  . (tpgaDefaultNotation ++) . (' ':) . render <$> tpgaDefaultValue]

instance PGArgumentClass TestPGArgument TestPGType where
  argumentMode     = fromMaybe In . tpgaMode
  argumentOptional = maybe False (const True) . tpgaDefaultValue
  argumentType     = tpgaType


newtype TestPGArgumentList = TestPGArgumentList { getArguments :: [TestPGArgument] } deriving (Show)

instance Arbitrary TestPGArgumentList where
  arbitrary = TestPGArgumentList <$> (listOf arbitrary)
  shrink (TestPGArgumentList xs) = map TestPGArgumentList (tail . tails $ xs)

instance PGSql TestPGArgumentList where
  render (TestPGArgumentList xs) = intercalate ", " . map render $ xs


wrap :: ArgumentListChecker TestPGArgument -> TestPGArgumentList -> Bool
wrap check (TestPGArgumentList xs) = either (const False) (const True) . check $ xs

newtype TPGALCorrect = TPGALCorrect { getArgumentList :: TestPGArgumentList } deriving (Show)

instance Arbitrary TPGALCorrect where
  arbitrary = TPGALCorrect <$> arbitrary
    `suchThat` (wrap checkExpectedDefaults)
    `suchThat` (wrap checkNotExpectedDefaults)
    `suchThat` (wrap checkVariadic)

instance PGSql TPGALCorrect where
  render (TPGALCorrect x) = render x


newtype TPGALFailedCheckExpectedDefaults = TPGALFailedCheckExpectedDefaults TestPGArgumentList deriving (Show)

instance Arbitrary TPGALFailedCheckExpectedDefaults where
  arbitrary = TPGALFailedCheckExpectedDefaults <$> arbitraryArgumentList
    `suchThat` (not . wrap checkExpectedDefaults)
    `suchThat` (wrap checkNotExpectedDefaults)
    `suchThat` (wrap checkVariadic)
    where
      arbitraryArgumentList = sized $ \n -> fmap (TestPGArgumentList . concat)
                                          . sequence . map (resize $ max 1 (n `div` 3)) $ [
          listOf  arbitrary
        , listOf1 (arbitrary `suchThat` (isJust . tpgaDefaultValue)) `suchThat` (not . empty')
        , listOf1 (arbitrary `suchThat` (isNothing . tpgaDefaultValue)) `suchThat` (not . empty')]

      empty' = null . filter ((`elem` [In, InOut]) . fromMaybe In  . tpgaMode)

instance PGSql TPGALFailedCheckExpectedDefaults where
  render (TPGALFailedCheckExpectedDefaults x) = render x


newtype TPGALFailedCheckNotExpectedDefaults = TPGALFailedCheckNotExpectedDefaults TestPGArgumentList deriving (Show)

instance Arbitrary TPGALFailedCheckNotExpectedDefaults where
  arbitrary = TPGALFailedCheckNotExpectedDefaults <$> arbitrary
    `suchThat` (wrap checkExpectedDefaults)
    `suchThat` (not . wrap checkNotExpectedDefaults)
    `suchThat` (wrap checkVariadic)

instance PGSql TPGALFailedCheckNotExpectedDefaults where
  render (TPGALFailedCheckNotExpectedDefaults x) = render x


newtype TPGALFailedCheckVariadic = TPGALFailedCheckVariadic TestPGArgumentList deriving (Show)

instance Arbitrary TPGALFailedCheckVariadic where
  arbitrary = TPGALFailedCheckVariadic <$> arbitrary
    `suchThat` (wrap checkExpectedDefaults)
    `suchThat` (wrap checkNotExpectedDefaults)
    `suchThat` (not . wrap checkVariadic)

instance PGSql TPGALFailedCheckVariadic where
  render (TPGALFailedCheckVariadic x) = render x


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


data TestPGTypeCast
  = TPGTCPrefix       TestPGType                TestPGString
  | TPGTCSuffix       TestPGType                TestPGExpression
  | TPGTCAs           TestPGType                TestPGExpression
  | TPGTCFunctionCall TestPGQualifiedIdentifier TestPGExpression
  deriving (Show, Eq)

instance Arbitrary TestPGTypeCast where
  arbitrary = oneof [
      TPGTCPrefix       <$> arbitrary <*> arbitrary
    , TPGTCSuffix       <$> arbitrary <*> arbitrary
    , TPGTCAs           <$> arbitrary <*> arbitrary
    , TPGTCFunctionCall <$> arbitrary <*> arbitrary]

instance PGSql TestPGTypeCast where
  render (TPGTCPrefix       t v) = (render t) ++ " " ++ (render v)
  render (TPGTCSuffix       t v) = (render v) ++ "::" ++ (render t)
  render (TPGTCAs           t v) = "cast (" ++ (render v) ++ " as " ++ (render t) ++ ")"
  render (TPGTCFunctionCall t v) = (render t) ++ "(" ++ (render v) ++ ")"


data TestPGExpression
  = TPGEConstant TestPGConstant
  | TPGEIdentifier TestPGQualifiedIdentifier
  | TPGEFunctionInvocation TestPGQualifiedIdentifier [TestPGExpression]
  | TPGETypeCast TestPGTypeCast
  deriving (Show, Eq)

instance Arbitrary TestPGExpression where
  arbitrary = sized $ \n -> oneof [
      TPGEConstant <$> arbitrary
    , TPGEIdentifier <$> arbitrary
    , TPGEFunctionInvocation <$> arbitrary <*> (listOf $ resize (min 1 (n `div` 2)) arbitrary)
    , TPGETypeCast <$> arbitrary]

instance PGSql TestPGExpression where
  render (TPGEConstant c) = render c
  render (TPGEIdentifier n) = render n
  render (TPGEFunctionInvocation n args) = (render n) ++ "(" ++ (intercalate "," . map render $ args) ++ ")"
  render (TPGETypeCast e) = render e


main :: IO ()
main = do
  samples <- loadDirectory "tests/samples"
  hspec (spec samples)


spec :: Map FilePath Text -> Spec
spec samples = do
  PGString.spec
  PGIdentifier.spec
  PGComment.spec

  describe "pgColumnType" $ do
    propParsingWorks pgColumnType (Proxy :: Proxy TestPGColumnType)

  describe "pgExactType" $ do
    propParsingWorks pgExactType (Proxy :: Proxy TestPGExactType)

  describe "pgType" $ do
    propParsingWorks pgType (Proxy :: Proxy TestPGType)

  describe "pgOperator" $ do
    propParsingWorks pgOperator (Proxy :: Proxy TestPGOperator)

  describe "pgResult" $ do
    propParsingWorks pgResult (Proxy :: Proxy TestPGResult)

  describe "pgArgument" $ do
    propParsingWorks pgArgument (Proxy :: Proxy TestPGArgument)

  describe "pgArgumentList" $ do
    propParsingWorks (pgArgumentList True) (Proxy :: Proxy TPGALCorrect)
    propParsingFails (pgArgumentList True) "DefaultValueExpected" (Proxy :: Proxy TPGALFailedCheckExpectedDefaults)
    propParsingFails (pgArgumentList True) "DefaultValueNotExpected" (Proxy :: Proxy TPGALFailedCheckNotExpectedDefaults)
    propParsingFails (pgArgumentList True) "NonOutVariableAfterVariadic" (Proxy :: Proxy TPGALFailedCheckVariadic)

--  describe "pgFunction" $ do
--    propParsingWorks pgFunction (Proxy :: Proxy TPGFCorrect)
--    propParsingFails pgFunction "NoReturnTypeInfo" (Proxy :: Proxy TPGFFailedNoReturnTypeInfo)
--    propParsingFails pgFunction "IncoherentReturnTypes" (Proxy :: Proxy TPGFFailedIncoherentReturnTypes)

  describe "pgExpression" $ do
    propParsingWorks pgExpression (Proxy :: Proxy TestPGExpression)

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
