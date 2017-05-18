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
  , pgFunction
  ) where


import Control.Applicative ((*>), (<*), (<|>))
import Control.Monad.Catch (MonadThrow(..), throwM)
import Data.Attoparsec.Text (Parser, char, string, decimal, skipSpace, asciiCI, sepBy)
import Data.Attoparsec.Text (takeWhile, takeWhile1, parseOnly, takeTill, inClass)
import Data.Text (Text, toLower, unpack, append, length)
import Prelude hiding (takeWhile, length)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGColumn(..), PGResult(..))
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..))


ss :: Parser ()
ss = skipSpace

-- | TODO
pgIdentifier :: Parser Text
pgIdentifier = do
  s1 <- takeWhile1 (inClass "a-zA-Z_")
  s2 <- takeWhile (inClass "a-zA-Z_0-9$")
  return $ toLower $ s1 `append` s2


-- | TODO
pgType :: Parser Text
pgType = toLower <$> (foldr1 (<|>) $
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
      _  -> base `append` " " `append` tz

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
      _  -> base `append` " " `append` fields


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
pgResult :: Parser PGResult
pgResult = ss *> asciiCI "returns" *> ss *> (fmap toLower $
      asciiCI "setof"
  <|> asciiCI "table"
  <|> pgType) >>= \case
      "setof" -> (PGSetOf . unpack) <$> (ss *> pgType)
      "table" -> PGTable <$> (ss *> char '(' *> pgColumn `sepBy` (char ',') <* ss <* char ')')
      t       -> return $ PGSingle (unpack t)


-- | TODO
pgColumn :: Parser PGColumn
pgColumn = do
  pgcName <- fmap unpack $ ss *> pgIdentifier
  pgcType <- fmap unpack $ ss *> pgType <* ss
  return PGColumn {..}


-- | TODO
pgArgument :: Parser PGArgument
pgArgument = do
  pgaName     <- fmap unpack $ ss *> pgIdentifier
  pgaType     <- fmap unpack $ ss *> pgType
  pgaOptional <- fmap (( > 0) . length) $ ss *> (
          (asciiCI "default" <|> string "=") *> (takeTill (inClass ",)"))
      <|> (string ""))
  -- WARNING: parsing default values requires ability to parse almost arbitraty expressions.
  -- Here is a quick and dirty implementation of the parser.
  return PGArgument {..}


-- | TODO
pgFunction :: Parser PGFunction
pgFunction = do
  _            <- asciiCI "function"
  pgfSchema    <- fmap unpack $ ss *> ((pgIdentifier <* (char '.')) <|> (string ""))
  pgfName      <- fmap unpack $ ss *> pgIdentifier
  pgfArguments <- ss *> char '(' *> (pgArgument `sepBy` (char ','))
  pgfResult    <- ss *> char ')' *> pgResult
  _            <- ss
  return PGFunction {..}


-- | Takes PostgreSQL function signature and represent it as an algebraic data type.
parsePGFunction :: (MonadThrow m) => Text -> m PGFunction
parsePGFunction s = either
  (\err -> throwM . ParserFailed . concat $ ["In declaration `", unpack s, "`: ", err])
  return
  (parseOnly (ss *> pgFunction) s) where
