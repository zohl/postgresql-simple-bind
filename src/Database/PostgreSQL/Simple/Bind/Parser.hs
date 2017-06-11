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
  , pgIdentifier
  , pgType
  , pgResult
  , pgString
  , pgColumn
  , pgArgument
  , pgArgumentMode
  , pgFunction
  , ParserException(..)
  ) where


import Control.Applicative ((*>), (<*), (<|>), liftA2, many)
import Control.Arrow ((&&&), second)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(..), throwM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Monoid ((<>))
import Data.Attoparsec.Text (Parser, char, string, skipSpace, asciiCI, sepBy, decimal)
import Data.Attoparsec.Text (takeWhile, takeWhile1, parseOnly, inClass, space, peekChar, satisfy, anyChar)
import Data.Default (def)
import Data.Text (Text)
import Prelude hiding (takeWhile, length)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResult(..), isPGTuple)
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..))
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
  deriving (Show)


ss :: Parser ()
ss = skipSpace

asciiCIs :: [Text] -> Parser ()
asciiCIs []     = return ()
asciiCIs (w:ws) = asciiCI w *> ss *> asciiCIs ws


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


-- | Parser for a tag of dollar-quoted string literal.
pgTag :: Parser Text
pgTag = pgNonEmptyTag <|> (pure "") where
  pgNonEmptyTag :: Parser Text
  pgNonEmptyTag = liftA2 T.cons (satisfy $ inClass "a-zA-Z_") (takeWhile $ inClass "a-zA-Z0-9_")

-- | Parser for a string.
pgString :: Parser Text
pgString = anyChar >>= \case
   '\'' -> pgQuotedString '\''
   '"'  -> pgQuotedString '"'
   '$'  -> liftA2 T.snoc (fmap (T.cons '$') $ pgTag) (char '$')
           >>= \tag -> fmap (tag <>) (pgDollarQuotedString tag)
   c    -> fail . show $ QuoteNotSupported c


-- | Parser for a generic identifier.
pgIdentifier :: Parser Text
pgIdentifier = ((char '"') *> (pgQuotedString '"')) <|> pgNormalIdentifier where
  pgNormalIdentifier = T.toLower <$> liftA2 T.cons
    (satisfy $ inClass "a-zA-Z_")
    (takeWhile $ inClass "a-zA-Z0-9_$")

-- | Parser for a type.
pgType :: Parser (Text, Maybe Text)
pgType = pgColumnType <|> pgExactType where

  pgColumnType :: Parser (Text, Maybe Text)
  pgColumnType = fmap ((,Nothing) . mconcat) . sequence $ [
      pgIdentifier
    , T.singleton <$> char '.'
    , pgIdentifier
    , (asciiCI "%type")]

  pgExactType :: Parser (Text, Maybe Text)
  pgExactType = do
    (typeName, typeModifiers) <-pgTime <|> (liftA2 (,) pgTypeName (ss *> pgTypeModifier))
    dimensions <- pgDimensions
    return (typeName <> dimensions, typeModifiers)

  pgTypeName :: Parser Text
  pgTypeName = pgInterval
           <|> asciiCI "double precision"
           <|> asciiCI "bit varying"
           <|> asciiCI "character varying"
           <|> pgIdentifier

  pgTypeModifier :: Parser (Maybe Text)
  pgTypeModifier = ((char '(') *> (Just <$> pgTypeModifier') <* (char ')'))
               <|> pure Nothing where
    pgTypeModifier' = pgString <|> takeWhile1 (/= ')')


  pgInterval :: Parser Text
  pgInterval = liftA2 (*<>) (asciiCI "interval" <* ss) (
        asciiCI "year to month"
    <|> asciiCI "day to hour"
    <|> asciiCI "day to minute"
    <|> asciiCI "day to second"
    <|> asciiCI "hour to minute"
    <|> asciiCI "hour to second"
    <|> asciiCI "minute to second"
    <|> asciiCI "year"
    <|> asciiCI "month"
    <|> asciiCI "day"
    <|> asciiCI "hour"
    <|> asciiCI "minute"
    <|> asciiCI "second"
    <|> asciiCI "")

  pgTime :: Parser (Text, Maybe Text)
  pgTime = do
    (base, modifier, zone) <- (,,)
      <$> (asciiCI "timestamptz"
       <|> asciiCI "timestamp"
       <|> asciiCI "timetz"
       <|> asciiCI "time")
      <*> (ss *> pgTypeModifier)
      <*> (ss *> (asciiCI "with time zone" <|> asciiCI "without time zone" <|> string ""))
    pure (base *<> zone, modifier)

  (*<>) l r = if T.null r then l else l <> T.singleton ' ' <> r

  pgDimensions :: Parser Text
  pgDimensions = fmap ((flip T.replicate) "[]") $
        (asciiCI "array" *> ss *> (dimension *> (pure 1) <|> (pure 1)))
    <|> (fmap P.length $ many (ss *> dimension))
    <|> (pure 0)
    where
      dimension :: Parser (Maybe Int)
      dimension = (char '[') *> ((Just <$> decimal) <|> pure Nothing) <* (char ']')

-- | Parser for column definition in RETURNS TABLE clause.
pgColumn :: Parser PGColumn
pgColumn = liftA2 PGColumn
  (T.unpack <$> pgIdentifier)
  ((T.unpack . fst) <$> (ss *> pgType))

-- | Parser for an argument mode.
pgArgumentMode :: Parser PGArgumentMode
pgArgumentMode =
      (asciiCI "inout"    *> space *> return InOut)
  <|> (asciiCI "in"       *> space *> return In)
  <|> (asciiCI "out"      *> space *> return Out)
  <|> (asciiCI "variadic" *> space *> return Variadic)
  <|> (return def)


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
        (,,) <$> (ss *> pgArgumentName) <*> (ss *> pgArgumentType) <*> (ss *> pgOptional)
    <|> (,,) <$> (return Nothing)       <*> (ss *> pgArgumentType) <*> (ss *> pgOptional)
  return PGArgument {..} where

    pgArgumentName = fmap (Just . T.unpack) $ ss *> pgIdentifier

    pgArgumentType = fmap (T.unpack . fst) $ ss *> pgType

    pgOptional = ss *> (
          ((asciiCI "default" <|> string "=") *> ((not . T.null) <$> pgExpression))
      <|> (peekChar >>= \mc -> do
              when (maybe False (not . inClass ",)") mc) $ fail "TODO"
              return False))


-- | Parser for 'pg_get_function_result' output.
pgResult :: Parser PGResult
pgResult = (fmap T.toLower $ asciiCI "setof" <|> asciiCI "table" <|> (fst <$> pgType)) >>= \case
  "setof" -> (PGSetOf . T.unpack) <$> (ss *> (fst <$> pgType))
  "table" -> PGTable <$> (ss *> char '(' *> (ss *> pgColumn <* ss) `sepBy` (char ',') <* ss <* char ')')
  t       -> return $ PGSingle (T.unpack t)

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
      [a] -> Just . PGSingle . pgaType $ a
      as  -> Just . PGTuple . map (pgaType) $ as

    mergeResults :: Maybe PGResult -> Maybe PGResult -> Parser (Maybe PGResult)
    mergeResults mres mres' = case (liftA2 (==) mres mres') of
      Nothing    -> return (mres <|> mres')
      Just False -> runMaybeT $ do
        res  <- MaybeT . pure $ mres
        res' <- MaybeT . pure $ mres'
        if (res == (PGSingle "record") && isPGTuple res')
          then return res'
          else MaybeT (fail . show $ IncoherentReturnTypes res res')
      _          -> return mres

-- | Parser for a function.
pgFunction :: Parser PGFunction
pgFunction = do
  _            <- asciiCIs ["create", "function"] <|> asciiCIs ["create", "or", "replace", "function"]
  (pgfSchema, pgfName) <- ss *> (
        ((,) <$> (fmap (Just . T.unpack) $ pgIdentifier) <*> (char '.' *> (T.unpack <$> pgIdentifier)))
    <|> ((Nothing,) . T.unpack <$> pgIdentifier))

  pgfArguments' <- ss *> char '(' *> ((ss *> pgArgument <* ss) `sepBy` (char ',')) <* char ')'
  pgfResult'    <- ss *> (asciiCI "returns" *> ss *> (Just <$> pgResult)) <|> (return Nothing)
  _             <- ss *> "as" *> ss *> pgString

  (pgfArguments, pgfResult) <- normalizeFunction pgfArguments' pgfResult'

  return PGFunction {..}


-- | Takes PostgreSQL function signature and represent it as an algebraic data type.
parsePGFunction :: (MonadThrow m) => Text -> m PGFunction
parsePGFunction s = either
  (\err -> throwM . ParserFailed . concat $ ["In declaration `", T.unpack s, "`: ", err])
  return
  (parseOnly (ss *> pgFunction) s) where
