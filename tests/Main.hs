{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Char (chr, isNumber, toLower)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, intercalate, tails)
import Data.Maybe (listToMaybe, fromMaybe, catMaybes, isJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..), tagWith)
import Data.Either (isLeft, isRight)
import Control.Monad (liftM2)
import Data.Attoparsec.Text (Parser, parseOnly, endOfInput)
import Data.Default (def)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgumentClass(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResultClass(..), PGResult(..), PGIdentifier(..), PGTypeClass(..))
import Text.Heredoc (str)
import Test.Hspec (Expectation, Spec, hspec, describe, it, shouldSatisfy, shouldBe, expectationFailure)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, choose, suchThat, frequency, arbitrarySizedNatural, listOf, listOf1, elements, arbitraryBoundedEnum)
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

charOperator :: Gen Char
charOperator = inClass "+*/<>=~!@#%^&|`?-"

charId' :: (Gen Char, Gen Char)
charId' = (inClass "A-Za-z_", inClass "A-Za-z0-9_$")

charTag' :: (Gen Char, Gen Char)
charTag' = (inClass "A-Za-z_", inClass "A-Za-z0-9_")


arbitrarySumDecomposition :: Int -> Gen [Int]
arbitrarySumDecomposition 0 = return []
arbitrarySumDecomposition n = choose (1, n) >>= \k -> (k:) <$> (arbitrarySumDecomposition (n-k))


data TestPGTag = TestPGTag String deriving (Show)

instance Arbitrary TestPGTag where
  arbitrary = TestPGTag <$> oneof [arbitraryString' charTag', return ""] where

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
  arbitrary = TestPGNormalIdentifier <$> (arbitraryString' charId') `suchThat` (not . null)

instance PGSql TestPGNormalIdentifier where
  render (TestPGNormalIdentifier s) = s


data TestPGIdentifier = TestPGIdentifier String deriving (Show, Eq)

instance Arbitrary TestPGIdentifier where
  arbitrary = TestPGIdentifier <$> oneof [
      render <$> (arbitrary :: Gen TestPGNormalIdentifier)
    , (('"':) . render <$> (arbitrary :: Gen (TestPGQuotedString "\""))) `suchThat` ((> 2) . length)]

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


data TestPGType = TestPGType String deriving (Show, Eq)

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
  , tpgaDefaultValue    :: Maybe String
  } deriving (Show, Eq)

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

instance PGArgumentClass TestPGArgument TestPGType where
  argumentMode     = fromMaybe In . tpgaMode
  argumentOptional = maybe False (const True) . tpgaDefaultValue
  argumentType     = tpgaType


data TestPGArgumentList = TestPGArgumentList { getArguments :: [TestPGArgument] } deriving (Show)

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

-- instance Show TestPGFunction where
--   show x = render x

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
  deriving (Show)

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
  deriving (Show)

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
  deriving (Show)

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

  describe "pgNormalIdentifier" $ do
    propParsingWorks pgNormalIdentifier (Proxy :: Proxy TestPGNormalIdentifier)
    let prop' = propParserRight (tagWith (Proxy :: Proxy TestPGNormalIdentifier) pgNormalIdentifier)
    prop' "the first symbol is not '$'" . flip shouldSatisfy $ \s -> (T.head s) /= '$'
    prop' "stored in lowercase" $ \x -> x `shouldBe` (T.toLower x)

  describe "pgIdentifier" $ do
    propParsingWorks pgIdentifier (Proxy :: Proxy TestPGIdentifier)

  describe "pgQualifiedIdentifier" $ do
    propParsingWorks pgQualifiedIdentifier (Proxy :: Proxy TestPGQualifiedIdentifier)

  describe "pgLineComment" $ do
    propParsingWorks pgLineComment (Proxy :: Proxy TestPGLineComment)

  describe "pgBlockComment" $ do
    propParsingWorks pgBlockComment (Proxy :: Proxy TestPGBlockComment)

    let prop' = propParserRight (tagWith (Proxy :: Proxy TestPGBlockComment) pgBlockComment)
    prop' "starts with /*" . flip shouldSatisfy $ T.isPrefixOf "/*"
    prop' "ends with */" . flip shouldSatisfy $ T.isSuffixOf "*/"

  describe "pgComment" $ do
    propParsingWorks pgComment (Proxy :: Proxy TestPGComment)

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
