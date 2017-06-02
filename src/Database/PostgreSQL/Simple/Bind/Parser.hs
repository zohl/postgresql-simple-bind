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
  ) where


import Control.Applicative ((*>), (<*), (<|>), liftA2, many)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(..), throwM)
import Data.Monoid ((<>))
import Data.Attoparsec.Text (Parser, char, string, skipSpace, asciiCI, sepBy, decimal)
import Data.Attoparsec.Text (takeWhile, takeWhile1, parseOnly, inClass, space, peekChar, satisfy, anyChar)
import Data.Default (def)
import Data.Text (Text)
import Prelude hiding (takeWhile, length)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResult(..))
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..))
import qualified Data.Text as T
import qualified Prelude as P


ss :: Parser ()
ss = skipSpace


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
   _    -> fail "TODO: quote not supported"


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
    pgTypeModifier' = takeWhile1 (/= ')')  -- TODO: can be arbitrary string or identifier


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

-- | TODO
pgColumn :: Parser PGColumn
pgColumn = do
  pgcName <- fmap T.unpack $ ss *> pgIdentifier
  pgcType <- fmap (T.unpack . fst) $ ss *> pgType <* ss
  return PGColumn {..}

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
  "table" -> PGTable <$> (ss *> char '(' *> pgColumn `sepBy` (char ',') <* ss <* char ')')
  t       -> return $ PGSingle (T.unpack t)

-- | TODO
pgFunction :: Parser PGFunction
pgFunction = do
  _            <- asciiCI "function"
  pgfSchema    <- fmap T.unpack $ ss *> ((pgIdentifier <* (char '.')) <|> (string ""))
  pgfName      <- fmap T.unpack $ ss *> pgIdentifier
  pgfArguments <- ss *> char '(' *> (pgArgument `sepBy` (char ','))
  pgfResult    <- ss *> char ')' *> ss *> asciiCI "returns" *> ss *> pgResult
  _            <- ss
  return PGFunction {..}


-- | Takes PostgreSQL function signature and represent it as an algebraic data type.
parsePGFunction :: (MonadThrow m) => Text -> m PGFunction
parsePGFunction s = either
  (\err -> throwM . ParserFailed . concat $ ["In declaration `", T.unpack s, "`: ", err])
  return
  (parseOnly (ss *> pgFunction) s) where
