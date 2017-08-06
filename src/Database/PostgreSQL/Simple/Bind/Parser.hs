{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

{-|
  Module:      Database.PostgreSQL.Simple.Bind.Representation
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  Parsers for sql-code.
-}

module Database.PostgreSQL.Simple.Bind.Parser (
    parsePGFunction

  , pgTag
  , pgQuotedString
  , pgDollarQuotedString
  , pgString

  , pgNormalIdentifier
  , pgIdentifier
  , pgQualifiedIdentifier

  , pgLineComment
  , pgBlockComment
  , pgComment

  , pgColumnType
  , pgExactType
  , pgType

  , pgResult

  , pgExpression
  , pgArgument

  , ArgumentListChecker
  , checkExpectedDefaults
  , checkNotExpectedDefaults
  , checkVariadic
  , pgArgumentList

  , pgFunction
  , pgDeclarations
  , ParserException(..)
  ) where


import Control.Applicative ((*>), (<*), (<|>), liftA2, many)
import Control.Arrow ((&&&), (***), first, second)
import Control.Monad (when, liftM2)
import Control.Monad.Catch (MonadThrow(..), throwM)
import Data.Maybe (listToMaybe, catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Attoparsec.Text (Parser, char, string, skipSpace, asciiCI, sepBy, decimal)
import Data.Attoparsec.Text (takeWhile, takeWhile1, parseOnly, inClass, space, peekChar, satisfy, anyChar)
import Data.Attoparsec.Text (isEndOfLine, endOfLine, peekChar')
import Data.Default (def)
import Data.Text (Text)
import Prelude hiding (takeWhile, length)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResult(..), PGIdentifier(..), PGType(..), mergePGResults)
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..))
import Safe (tailSafe)
import qualified Data.Text as T
import qualified Prelude as P


-- | Exceptions thrown by parsers.
data ParserException
  = NoReturnTypeInfo
    -- ^ Thrown when function has no 'RETURNS' clause and no 'OUT'
    -- arguments.
  | IncoherentReturnTypes PGResult PGResult
    -- ^ Thrown when function has incoherent 'RETURNS' clause and 'OUT'
    -- arguments.
  | QuoteNotSupported Char
    -- ^ Thrown when string literal starts with non-supported symbol.
  | NonOutVariableAfterVariadic PGArgument
    -- ^ Thrown when encountered a non-OUT argument after VARIADIC one.
  | DefaultValueExpected PGArgument
    -- ^ Thrown when encountered an argument that must have
    -- default value (i.e. after another argument with default value).
  | DefaultValueNotExpected PGArgument
    -- ^ Thrown when encountered an argument that must NOT have
    -- default value (i.e. VARIADIC or OUT).
  deriving (Show)


ss :: Parser ()
ss = skipSpace

asciiCIs :: [Text] -> Parser Text
asciiCIs = fmap (T.intercalate " ") . asciiCIs' where
  asciiCIs' :: [Text] -> Parser [Text]
  asciiCIs' [] = pure []
  asciiCIs' ws = (uncurry (liftA2 (:))) . ((( <* ss) . asciiCI . head) &&& (asciiCIs' . tail)) $ ws

withParentheses :: Parser a -> Parser a
withParentheses p = (char '(') *> p <* (char ')')

withSpaces :: Parser a -> Parser a
withSpaces p = ss *> p <* ss


-- | Parser for a tag of dollar-quoted string literal.
pgTag :: Parser Text
pgTag = pgNonEmptyTag <|> (pure "") where
  pgNonEmptyTag :: Parser Text
  pgNonEmptyTag = liftA2 T.cons (satisfy $ inClass "a-zA-Z_") (takeWhile $ inClass "a-zA-Z0-9_")


-- | Parser for a string surrounded by "'" or "\"".
pgQuotedString :: Char -> Parser Text
pgQuotedString q = do
  (s, mq) <- (,)
    <$> (liftA2 (<>)
         (T.cons q <$> (takeWhile (/= q)))
         (T.singleton <$> anyChar))
    <*> peekChar
  case mq of
    Nothing -> return s
    Just c  -> if c == q
               then liftA2 (<>) (return s) (anyChar *> pgQuotedString q)
               else return s


-- | Parser for a dollar-quoted string.
pgDollarQuotedString :: Text -> Parser Text
pgDollarQuotedString q = do
  (s, t) <- liftA2 (,)
    (takeWhile (/= '$'))
    (string q <|> (fmap T.singleton $ char '$'))
  if t == q
    then return (s <> t)
    else fmap ((T.snoc s '$') <>) (pgDollarQuotedString q)


-- | Parser for a string.
pgString :: Parser Text
pgString = anyChar >>= \case
   '\'' -> pgQuotedString '\''
   '"'  -> pgQuotedString '"'
   '$'  -> liftA2 T.snoc (fmap (T.cons '$') $ pgTag) (char '$')
           >>= \tag -> fmap (tag <>) (pgDollarQuotedString tag)
   c    -> fail . show $ QuoteNotSupported c


-- | Parser for a non-quoted identifier.
pgNormalIdentifier :: Parser Text
pgNormalIdentifier = T.toLower <$> liftA2 T.cons
  (satisfy $ inClass "a-zA-Z_")
  (takeWhile $ inClass "a-zA-Z0-9_$")


-- | Parser for a generic identifier.
pgIdentifier :: Parser Text
pgIdentifier = ((char '"') *> (pgQuotedString '"')) <|> pgNormalIdentifier where


-- | Parser combinator to retrieve additionally a qualification.
qualified :: Parser a -> Parser (Text, a)
qualified = liftA2 (,) (pgIdentifier <* char '.')


-- | Parser combinator to retrieve additionally a qualification (if exist).
optionallyQualified :: Parser a -> Parser (Maybe Text, a)
optionallyQualified p = ((first Just) <$> qualified p) <|> ((Nothing,) <$> p)


-- | Parser for an (optionally) qualified identifier.
pgQualifiedIdentifier :: Parser PGIdentifier
pgQualifiedIdentifier = uncurry PGIdentifier . (fmap T.unpack *** T.unpack) <$> optionallyQualified pgIdentifier


-- | Parser for a line comment.
pgLineComment :: Parser Text
pgLineComment = (liftA2 (<>) (string "--") (takeWhile (not . isEndOfLine))) <* (endOfLine <|> pure ())


-- | Parser for a block comment.
pgBlockComment :: Parser Text
pgBlockComment = liftA2 (<>) (string "/*") pgBlockCommentTail where
  pgBlockCommentTail = do
    s <- takeWhile (\c -> (c /= '/') && (c /= '*'))
    fmap (s <>) $ foldl1 (<|>) [
        (liftA2 (<>) pgBlockComment pgBlockCommentTail)
      , (string "*/")
      , (T.cons <$> anyChar <*> pgBlockCommentTail)]


-- | Parser for a comment.
pgComment :: Parser Text
pgComment = pgLineComment <|> pgBlockComment


-- | Parser for a column type.
pgColumnType :: Parser (Text, Maybe Text)
pgColumnType = ((,Nothing) . uncurry (<>) . ((<> ".") *** (<> "%type")))
           <$> (qualified pgIdentifier <* asciiCI "%type")


-- | Parser for an exact type.
pgExactType :: Parser (Text, Maybe Text)
pgExactType = do
  name       <- ss *> pgTypeName
  modifier   <- ss *> pgTypeModifier
  timeZone   <- if ((T.toLower name) `elem` ["time", "timestamp"])
                  then ss *> pgTimeZone
                  else pure Nothing
  dimensions <- ss *> pgDimensions

  return $ (
      T.concat [name, fromMaybe T.empty (T.cons ' ' <$> timeZone), dimensions]
    , modifier
    ) where
    pgTypeName :: Parser Text
    pgTypeName = foldr1 (<|>) ((map asciiCIs multiWordIdentifiers) ++ [pgIdentifier])

    multiWordIdentifiers :: [[Text]]
    multiWordIdentifiers = [
        ["double", "precision"]
      , ["bit", "varying"]
      , ["character", "varying"]
      ] ++ (map ("interval":) [
        ["year", "to", "month"]
      , ["day", "to", "hour"]
      , ["day", "to", "minute"]
      , ["day", "to", "second"]
      , ["hour", "to", "minute"]
      , ["hour", "to", "second"]
      , ["minute", "to", "second"]
      , ["year"]
      , ["month"]
      , ["day"]
      , ["hour"]
      , ["minute"]
      , ["second"]])

    pgTypeModifier :: Parser (Maybe Text)
    pgTypeModifier = (withParentheses $ Just <$> pgTypeModifier')
                 <|> pure Nothing where
      pgTypeModifier' = pgString <|> takeWhile1 (/= ')')

    pgTimeZone :: Parser (Maybe Text)
    pgTimeZone = (Just <$> asciiCIs ["with", "time", "zone"])
             <|> (Just <$> asciiCIs ["without", "time", "zone"])
             <|> pure Nothing

    pgDimensions :: Parser Text
    pgDimensions = fmap ((flip T.replicate) "[]") $
          (asciiCI "array" *> ss *> (dimension *> (pure 1) <|> (pure 1)))
      <|> (fmap P.length $ many (ss *> dimension))
      <|> (pure 0)
      where
        dimension :: Parser (Maybe Int)
        dimension = (char '[') *> ((Just <$> decimal) <|> pure Nothing) <* (char ']')


-- | Parser for a type.
pgType :: Parser PGType
pgType = restructure
       . first (fmap T.unpack)
       . second (T.unpack *** fmap T.unpack)
       <$> (optionallyQualified pgColumnType <|> optionallyQualified pgExactType) where

  restructure :: (Maybe String, (String, Maybe String)) -> PGType
  restructure (pgiSchema, (pgiName, pgtModifiers)) = let pgtIdentifier = PGIdentifier {..} in PGType {..}


-- | Parser for 'pg_get_function_result' output.
pgResult :: Parser PGResult
pgResult = ss *> (pgResultSetOf <|> pgResultTable <|> pgResultSingle) where

  pgResultSetOf :: Parser PGResult
  pgResultSetOf = fmap (PGSetOf . pure) $ asciiCI "setof" *> ss *> pgType

  pgResultTable :: Parser PGResult
  pgResultTable = fmap PGTable $ asciiCI "table" *> ss *> (withParentheses $ (withSpaces pgColumn) `sepBy` (char ',') <* ss)

  pgColumn :: Parser PGColumn
  pgColumn = liftA2 PGColumn
    (T.unpack <$> pgIdentifier)
    (ss *> pgType)

  pgResultSingle :: Parser PGResult
  pgResultSingle = fmap (PGSingle . pure) $ pgType


-- | Parser for an expression (as a default value for an argument).
-- WARNING: parsing default values requires ability to parse almost arbitraty expressions.
-- Here is a quick and dirty implementation of the parser.
pgExpression :: Parser Text
pgExpression = ss *> takeWhile (not . inClass ",)")


-- | Parser for a single argument in a function declaration.
pgArgument :: Parser PGArgument
pgArgument = do
  pgaMode <- ss *> pgArgumentMode
  (pgaName, pgaType, pgaOptional) <-
        (,,) <$> (ss *> pgArgumentName) <*> (ss *> pgType) <*> (ss *> pgOptional)
    <|> (,,) <$> (return Nothing)       <*> (ss *> pgType) <*> (ss *> pgOptional)
  return PGArgument {..} where

    pgArgumentMode :: Parser PGArgumentMode
    pgArgumentMode =
          (asciiCI "inout"    *> space *> return InOut)
      <|> (asciiCI "in"       *> space *> return In)
      <|> (asciiCI "out"      *> space *> return Out)
      <|> (asciiCI "variadic" *> space *> return Variadic)
      <|> (return def)

    pgArgumentName :: Parser (Maybe String)
    pgArgumentName = fmap (Just . T.unpack) $ ss *> pgIdentifier

    pgOptional :: Parser Bool
    pgOptional = ss *> (
          ((asciiCI "default" <|> string "=") *> ((not . T.null) <$> pgExpression))
      <|> (peekChar >>= \mc -> do
              when (maybe False (not . inClass ",)") mc) $ fail "TODO"
              return False))

type ArgumentListChecker t
  =  (t -> PGArgumentMode)  -- ^ Mode of an argument.
  -> (t -> Bool)            -- ^ Is an argument optional?
  -> [t]                    -- ^ Argument list.
  -> Maybe t                -- ^ First argument, that violates the test.

-- | Checks the following property of an argument list:
--   all input arguments following an argument with a default value
--   must have default values as well.
checkExpectedDefaults :: ArgumentListChecker t
checkExpectedDefaults mode optional
  = listToMaybe
  . dropWhile optional
  . dropWhile (not . optional)
  . filter ((`elem` [In, InOut]) . mode)


-- | Checks the following property of an argument list:
--   only input arguments can have default values.
checkNotExpectedDefaults :: ArgumentListChecker t
checkNotExpectedDefaults mode optional
  = listToMaybe
  . filter optional
  . filter ((`elem` [Out, Variadic]) . mode)


-- | Checks the following property of an argument list:
--   only OUT arguments can follow VARIADIC one.
checkVariadic :: ArgumentListChecker t
checkVariadic  mode _
  = listToMaybe
  . filter ((/= Out) . mode)
  . tailSafe . snd
  . break ((== Variadic) . mode)


-- | Parser for an argument list as in 'pg_catalog.pg_get_function_result'.
pgArgumentList :: Bool -> Parser [PGArgument]
pgArgumentList doCheck = do
  args <- ((withSpaces pgArgument) `sepBy` (char ','))

  when doCheck $ do
    let check (t, e) = maybe (pure ()) (fail . show . e) (t pgaMode pgaOptional args)
    mapM_ check [
        (checkExpectedDefaults,    DefaultValueExpected)
      , (checkNotExpectedDefaults, DefaultValueNotExpected)
      , (checkVariadic,            NonOutVariableAfterVariadic)]

  return args


-- | Move 'Out' arguments to PGResult record.
normalizeFunction :: [PGArgument] -> Maybe PGResult -> Parser ([PGArgument], PGResult)
normalizeFunction args mr = do
  let (iArgs, mres') = second mkResult $ splitArgs args
  r <- mergeResults mr mres' >>= maybe (fail . show $ NoReturnTypeInfo) return
  return (iArgs, r) where

    splitArgs :: [PGArgument] -> ([PGArgument], [PGArgument])
    splitArgs = (filter ((/= Out) . pgaMode)) &&& (filter ((flip elem [Out, InOut]) . pgaMode))

    mkResult :: [PGArgument] -> Maybe PGResult
    mkResult = \case
      []  -> Nothing
      as  -> Just . PGSingle . map (pgaType) $ as

    mergeResults :: Maybe PGResult -> Maybe PGResult -> Parser (Maybe PGResult)
    mergeResults mres mres' = maybe
     (return $ mres <|> mres')
     (\(res, res') -> maybe
       (fail . show $ IncoherentReturnTypes res res')
       (return . Just)
       (mergePGResults res res'))
     (liftA2 (,) mres mres')

-- | Parser for a function property.
pgFunctionProperty :: Parser ()
pgFunctionProperty =
      language       *> pure ()
  <|> loadableObject *> pure ()
  <|> definition     *> pure ()
  <|> window         *> pure ()
  <|> behaviour      *> pure ()
  <|> leakproof      *> pure ()
  <|> strictness     *> pure ()
  <|> security       *> pure ()
  <|> parallel       *> pure ()
  <|> cost           *> pure ()
  <|> rows           *> pure ()
  <|> transform      *> pure ()
  <|> set            *> pure ()
  where
    language       = asciiCI "language" *> ss *> (pgNormalIdentifier <|> (char '\'' *> pgQuotedString '\''))
    loadableObject = asciiCI "as" *> ss *> pgString *> ss *> char ',' *> ss *> pgString
    definition     = asciiCI "as" *> ss *> pgString
    window         = asciiCI "window"
    behaviour      = asciiCI "immutable" <|> asciiCI "stable" <|> asciiCI "volatile"
    leakproof      = ((asciiCI "not" *> ss *> pure ()) <|> pure ()) *> asciiCI "leakproof"
    strictness     = asciiCIs ["called", "on", "null", "input"] *> pure ()
                 <|> asciiCIs ["returns", "null", "on", "null", "input"] *> pure ()
                 <|> asciiCI "strict" *> pure ()
    security       = ((asciiCI "external" *> ss *> pure ()) <|> pure ())
                  *> asciiCI "security" *> ss *> (asciiCI "invoker" <|> asciiCI "definer")
    parallel       = asciiCI "parallel" *> ss *> (asciiCI "unsafe" <|> asciiCI "restricted" <|> asciiCI "safe")
    cost           = asciiCI "cost" *> ss *> (decimal :: Parser Int)
    rows           = asciiCI "rows" *> ss *> (decimal :: Parser Int)
    transform      = asciiCI "transform" *> ((ss *> asciiCIs ["for", "type"] *> ss *> optionallyQualified pgExactType) `sepBy` (char ','))
    set            = asciiCI "set" *> ss *> pgNormalIdentifier *> ss *> (
                         (asciiCI "to" *> ss *> value        *> pure ())
                     <|> (string "="   *> ss *> value        *> pure ())
                     <|> (asciiCIs ["from", "current"] *> ss *> pure ()))
      where
        value  = ((withSpaces value') `sepBy` (char ',')) <|> (pure <$> value')
        value' = pgString                *> pure ()
             <|> pgQualifiedIdentifier   *> pure ()
             <|> (decimal :: Parser Int) *> pure ()


-- | Parser for an obsolete function property (WITH clause).
pgFunctionObsoleteProperty :: Parser ()
pgFunctionObsoleteProperty = (asciiCI "isStrict" <|> asciiCI "isCachable") *> pure ()

-- | Parser for a function.
pgFunction :: Parser PGFunction
pgFunction = do
  _             <- asciiCIs ["create", "function"] <|> asciiCIs ["create", "or", "replace", "function"]
  pgfIdentifier <- ss *> pgQualifiedIdentifier

  (pgfArguments, pgfResult) <- liftM2 (,)
    (ss *> (withParentheses $ pgArgumentList True))
    (ss *> (asciiCI "returns" *> ss *> (Just <$> pgResult)) <|> (return Nothing))
    >>= uncurry normalizeFunction

  _ <- ss *> (pgFunctionProperty `sepBy` ss)
  _ <- ss *> (asciiCI "with" *> ((withSpaces pgFunctionObsoleteProperty) `sepBy` (char ',')) *> pure ()) <|> pure ()

  return PGFunction {..}


-- | Parser for a generic sql declarations. Filters out everything but
-- function declarations.
pgDeclarations :: Parser [PGFunction]
pgDeclarations = catMaybes <$> ((many (ss *> pgDeclaration)) <* ss) where

  pgDeclaration :: Parser (Maybe PGFunction)
  pgDeclaration = (Just <$> (pgFunction <* ss <* char ';'))
              <|> (pgNonFunction)

  pgNonFunction :: Parser (Maybe PGFunction)
  pgNonFunction = pgComment                       *> pure Nothing
              <|> pgOtherClause *> ss *> char ';' *> pure Nothing

  pgOtherClause :: Parser ()
  pgOtherClause = do
    (s, c) <- liftA2 (,)
      (takeWhile (not . inClass "'\"$;"))
      peekChar'
    if c == ';'
      then pure ()
      else if (inClass "'\"" c || T.null s || (not $ inClass "a-zA-Z0-9_" (T.last s)))
           then pgString *> pgOtherClause
           else anyChar  *> pgOtherClause


-- | Takes PostgreSQL function signature and represent it as an algebraic data type.
parsePGFunction :: (MonadThrow m) => Text -> m PGFunction
parsePGFunction s = either
  (\err -> throwM . ParserFailed . concat $ ["In declaration `", T.unpack s, "`: ", err])
  (return)
  (parseOnly (ss *> pgFunction) s) where
