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
  , pgColumn
  , pgArgument
  , pgArgumentMode
  , pgFunction
  ) where


import Control.Applicative ((*>), (<*), (<|>))
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(..), throwM)
import Data.Monoid ((<>))
import Data.Attoparsec.Text (Parser, char, string, decimal, skipSpace, asciiCI, sepBy)
import Data.Attoparsec.Text (takeWhile, takeWhile1, parseOnly, inClass, space, peekChar)
import Data.Default (def)
import Data.Text (Text)
import Prelude hiding (takeWhile, length)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResult(..))
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..))
import qualified Data.Text as T

ss :: Parser ()
ss = skipSpace

-- | TODO
pgIdentifier :: Parser Text
pgIdentifier = do
  s1 <- takeWhile1 (inClass "a-zA-Z_")
  s2 <- takeWhile (inClass "a-zA-Z_0-9$")
  return $ T.toLower $ s1 <> s2


-- | TODO
pgType :: Parser Text
pgType = T.toLower <$> (foldr1 (<|>) $
     (map asciiCI [ "double precision" ])
  ++ (map (\t -> (asciiCI t <* ss <* (modifiers $ Just 1))) ["bit", "character varying"])
  ++ (map (\t -> (asciiCI t <* ss <* (modifiers $ Just 2))) ["numeric", "decimal"])
  ++ ((asciiCI "timestamptz"):(map timeType ["timestamp", "time"]))
  ++ [intervalType, pgIdentifier <* (modifiers Nothing)]) where

  timeType t = do
    base <- asciiCI t <* ss <* (modifiers $ Just 1)
    tz <- ss *> ((asciiCI "with time zone") <|> (asciiCI "without time zone") <|> (string ""))
    return $ case tz of
      "" -> base
      _  -> base <> " " <> tz

  intervalType = do
    base <- (asciiCI "interval") <* ss
    fields <- foldr1 (<|>) $ map asciiCI [
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
      , "second"
      , ""]
    _ <- ss *> (modifiers $ Just 1)

    return $ case fields of
      "" -> base
      _  -> base <> " " <> fields


  modifiers limit = (char '(') *> exact <* (char ')') <|> less where
    exact = ($ limit) $ maybe
      ((modifier `sepBy` (char ',')) *> (string ""))
      (\n -> foldl1 ((*>) . (*> ((char ',') *> ss))) (replicate n modifier) *> string "")

    less = ($ limit) $ maybe
      (string "")
      (\n -> case n of
          1 -> (string "")
          _ -> (modifiers $ Just (n - 1)))

  modifier = (decimal :: Parser Int) *> ss


-- | TODO
pgColumn :: Parser PGColumn
pgColumn = do
  pgcName <- fmap T.unpack $ ss *> pgIdentifier
  pgcType <- fmap T.unpack $ ss *> pgType <* ss
  return PGColumn {..}

-- | Parser for argument mode.
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

    pgArgumentType = fmap T.unpack $ ss *> pgType

    pgOptional = ss *> (
          ((asciiCI "default" <|> string "=") *> ((not . T.null) <$> pgExpression))
      <|> (peekChar >>= \mc -> do
              when (maybe False (not . inClass ",)") mc) $ fail "TODO"
              return False))

-- | Parser for 'pg_get_function_result' output.
pgResult :: Parser PGResult
pgResult = (fmap T.toLower $ asciiCI "setof" <|> asciiCI "table" <|> pgType) >>= \case
  "setof" -> (PGSetOf . T.unpack) <$> (ss *> pgType)
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
