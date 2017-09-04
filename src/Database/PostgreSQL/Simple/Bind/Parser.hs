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

  , pgConstant

  , pgNormalIdentifier
  , pgIdentifier
  , pgQualifiedIdentifier

  , pgLineComment
  , pgBlockComment
  , pgComment

  , pgColumnType
  , pgExactType
  , pgType
  , pgOperator

  , pgResult

  , pgExpression
  , pgArgument

  , ArgumentListChecker
  , checkExpectedDefaults
  , checkNotExpectedDefaults
  , checkVariadic
  , pgArgumentList

  , normalizeFunction
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
import Data.Attoparsec.Text (Parser, char, string, skipSpace, asciiCI, sepBy, decimal, double)
import Data.Attoparsec.Text (takeWhile, takeWhile1, parseOnly, inClass, space, peekChar, satisfy, anyChar)
import Data.Attoparsec.Text (isEndOfLine, endOfLine, match)
import Data.Default (def)
import Data.Text (Text)
import Prelude hiding (takeWhile, length)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgumentClass(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResultClass(..),  PGResultClass(..), PGResult(..), PGIdentifier(..), PGTypeClass(..), PGType(..))
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..))
import Safe (tailSafe)
import qualified Data.Text as T
import qualified Prelude as P


-- | Exceptions thrown by parsers.
data ParserException
  = NoReturnTypeInfo
    -- ^ Thrown when function has no 'RETURNS' clause and no 'OUT'
    -- arguments.
  | IncoherentReturnTypes String String
    -- ^ Thrown when function has incoherent 'RETURNS' clause and 'OUT'
    -- arguments.
  | QuoteNotSupported Char
    -- ^ Thrown when string literal starts with non-supported symbol.
  | NonOutVariableAfterVariadic String
    -- ^ Thrown when encountered a non-OUT argument after VARIADIC one.
  | DefaultValueExpected String
    -- ^ Thrown when encountered an argument that must have
    -- default value (i.e. after another argument with default value).
  | DefaultValueNotExpected String
    -- ^ Thrown when encountered an argument that must NOT have
    -- default value (i.e. VARIADIC or OUT).
    deriving Show


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


-- | Parser for a constant.
pgConstant :: Parser Text
pgConstant = pgString
         <|> (fst <$> match double)


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
pgColumnType :: Parser (Text, [Text])
pgColumnType = ((,[]) . uncurry (<>) . ((<> ".") *** (<> "%type")))
           <$> (qualified pgIdentifier <* asciiCI "%type")


-- | Parser for an exact type.
pgExactType :: Parser (Text, [Text])
pgExactType = do
  name       <- ss *> pgTypeName
  modifiers  <- ss *> pgTypeModifiers
  timeZone   <- if ((T.toLower name) `elem` ["time", "timestamp"])
                  then ss *> pgTimeZone
                  else pure Nothing
  dimensions <- ss *> pgDimensions

  return $ (
      T.concat [name, fromMaybe T.empty (T.cons ' ' <$> timeZone), dimensions]
    , modifiers
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

    pgTypeModifiers :: Parser [Text]
    pgTypeModifiers = (withParentheses $
                        (ss *> ((fst <$> match pgQualifiedIdentifier) <|> pgConstant)) `sepBy` (ss *> char ','))
                  <|> pure []

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

  restructure :: (Maybe String, (String, [String])) -> PGType
  restructure (pgiSchema, (pgiName, pgtModifiers)) = let pgtIdentifier = PGIdentifier {..} in PGType {..}


-- | Parser for an operator name.
pgOperator :: Parser Text
pgOperator = do
  op <- takeWhile1 (inClass "+*/<>=~!@#%^&|`?-")
  when (T.isInfixOf "--" op) $ fail "operator cannot contain `--`"
  when (T.isInfixOf "/*" op) $ fail "operator cannot contain `/*`"
  when (((`elem` ("+-"::String)) . T.last $ op) && (T.null . T.filter (`elem` ("~!@#%^&|`?"::String)) $ op))
    $ fail "this operator cannot end in -,+"
  return op


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
  pgResultSingle = pgType >>= \t -> if t == "null"
                                    then fail "NULL is not a type"
                                    else return $ PGSingle [t]


-- | Parser for an expression (as a default value for an argument).
pgExpression :: Parser ()
pgExpression = pgExpression' True "" where
  pgExpression' :: Bool -> String -> Parser ()

  pgExpression' True [] = ss *> peekChar >>= \case
    Nothing  -> pure ()
    Just ')' -> pure ()
    Just ']' -> pure ()
    Just ',' -> pure ()
    Just ';' -> pure ()
    _        -> pgExpression' False []

  pgExpression' _ s = (ss *> pgToken) >>= updateState s >>= pgExpression' True

  updateState :: String -> Maybe Char -> Parser String
  updateState s       Nothing    = pure s
  updateState ('(':s) (Just ')') = pure s
  updateState ('[':s) (Just ']') = pure s
  updateState s       (Just c)   = if c `elem` (")]"::String)
    then fail "unbalanced parentheses"
    else pure (c:s)

  pgToken
    =   char '('              *> pure (Just '(')
    <|> char ')'              *> pure (Just ')')
    <|> char '['              *> pure (Just '[')
    <|> char ']'              *> pure (Just ']')
    <|> char ','              *> pure Nothing
    <|> string "::"           *> pure Nothing
    <|> pgOperator            *> pure Nothing
    <|> pgQualifiedIdentifier *> pure Nothing
    <|> pgConstant            *> pure Nothing

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
          ((asciiCI "default" <|> string "=") *> pgExpression *> pure True)
      <|> (peekChar >>= \mc -> do
              when (maybe False (not . inClass ",)") mc) $ fail "TODO"
              return False))


-- | A type of a function to check list of arguments.
type ArgumentListChecker a = [a] -> Either ParserException ()


-- | Checks the following property of an argument list:
--   all input arguments following an argument with a default value
--   must have default values as well.
checkExpectedDefaults :: (PGTypeClass t, PGArgumentClass a t) => ArgumentListChecker a
checkExpectedDefaults
  = maybe  (Right ()) (Left . DefaultValueExpected . show)
  . listToMaybe
  . dropWhile argumentOptional
  . dropWhile (not . argumentOptional)
  . filter ((`elem` [In, InOut]) . argumentMode)


-- | Checks the following property of an argument list:
--   only input arguments can have default values.
checkNotExpectedDefaults :: (PGTypeClass t, PGArgumentClass a t) => ArgumentListChecker a
checkNotExpectedDefaults
  = maybe (Right ()) (Left . DefaultValueNotExpected . show)
  . listToMaybe
  . filter argumentOptional
  . filter ((`elem` [Out, Variadic]) . argumentMode)


-- | Checks the following property of an argument list:
--   only OUT arguments can follow VARIADIC one.
checkVariadic :: (PGTypeClass t, PGArgumentClass a t) => ArgumentListChecker a
checkVariadic
  = maybe (Right ()) (Left . NonOutVariableAfterVariadic . show)
  . listToMaybe
  . filter ((/= Out) . argumentMode)
  . tailSafe . snd
  . break ((== Variadic) . argumentMode)


-- | Parser for an argument list as in 'pg_catalog.pg_get_function_result'.
pgArgumentList :: Bool -> Parser [PGArgument]
pgArgumentList doCheck = do
  args <- ((withSpaces pgArgument) `sepBy` (char ','))

  when doCheck $ do
    let check t = either (fail . show) (pure) (t args)
    mapM_ check [checkExpectedDefaults, checkNotExpectedDefaults, checkVariadic]

  return args


-- | Move 'Out' arguments to PGResult record.
normalizeFunction
  :: (PGTypeClass t, PGArgumentClass a t, PGResultClass r t)
  => ([a], Maybe r)
  -> Either ParserException ([a], r)
normalizeFunction (args, mr) = do
  let (inArgs, outArgs) = filter ((/= Out) . argumentMode) &&& filter ((`elem` [Out, InOut]) . argumentMode) $ args

  let mr' = if null outArgs
      then Nothing
      else Just (resultSingle $ map argumentType outArgs)

  r <- case (liftA2 (,) mr mr') of
         Nothing -> maybe
           (Left $ NoReturnTypeInfo)
           (Right)
           (mr <|> mr')
         Just (r, r') -> maybe
           (Left $ IncoherentReturnTypes (show r) (show r'))
           (Right)
           (mergeResults r r')

  return (inArgs, r)

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
        value  = (withSpaces value') `sepBy` (char ',')
        value' = pgQualifiedIdentifier   *> pure ()
             <|> pgString                *> pure ()
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
    >>= either (fail . show) return . normalizeFunction

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
  pgOtherClause = ss *> pgExpression


-- | Takes PostgreSQL function signature and represent it as an algebraic data type.
parsePGFunction :: (MonadThrow m) => Text -> m PGFunction
parsePGFunction s = either
  (\err -> throwM . ParserFailed . concat $ ["In declaration `", T.unpack s, "`: ", err])
  (return)
  (parseOnly (ss *> pgFunction) s) where
