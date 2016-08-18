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

{-|
  Module:      Database.PostgreSQL.Simple.Bind.Representation
  Copyright:   (c) 2016 Al Zohali
  License:     GPL3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  An algebraic data type that (partially) represents function declaration based on
  <http://www.postgresql.org/docs/9.5/static/sql-createfunction.html PostgreSQL documentation>.
-}

module Database.PostgreSQL.Simple.Bind.Representation (
    PGFunction(..)
  , PGArgument(..)
  , PGColumn(..)
  , PGResult(..)
  , parsePGFunction
  ) where


import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, toLower, unpack, append, length)
import Prelude hiding (takeWhile, length)


-- | Representation of a function's argument (name, type, is optional).
data PGArgument = PGArgument String String Bool deriving (Show, Eq)

-- | Representation of a PostrgeSQL function signature (schema, name, arguments, result).
data PGFunction = PGFunction String String [PGArgument] PGResult deriving (Show, Eq)

-- | Representation of a resultant's column (name, type).
data PGColumn = PGColumn String String deriving (Show, Eq)

-- | Representation of a function's return value.
data PGResult = PGSingle String
              | PGSetOf String
              | PGTable [PGColumn] deriving (Show, Eq)


-- | Takes PostgreSQL function signature and represent it as an algebraic data type.
parsePGFunction :: Text -> PGFunction
parsePGFunction s = either error id (parseOnly (ss *> function) s) where
  ss = skipSpace

  function = do
    _      <- asciiCI "function"
    schema <- ss *> ((identifier <* (char '.')) <|> (string ""))
    name   <- ss *> identifier
    args   <- ss *> char '(' *> (arguments `sepBy` (char ','))
    ret    <- ss *> char ')' *> returnType
    _      <- ss
    return $ PGFunction (unpack schema) (unpack name) args ret

  arguments = do
    name     <- ss *> identifier
    datatype <- ss *> postgresType
    def      <- ss *> ((asciiCI "default" <|> string "=") *> (takeTill (inClass ",)")) <|> (string ""))
    -- WARNING: parsing default values requires ability to parse almost arbitraty expressions.
    -- Here is a quick and dirty implementation of the parser.

    return $ PGArgument (unpack name) (unpack datatype) ((length def) > 0)

  cols = do
    name     <- ss *> identifier
    datatype <- ss *> postgresType <* ss
    return $ PGColumn (unpack name) (unpack datatype)

  returnType = do
    ret <- ss *> asciiCI "returns" *> ss *> (
               asciiCI "setof"
           <|> asciiCI "table"
           <|> postgresType)

    case toLower(ret) of
      "setof" -> (PGSetOf . unpack) <$> (ss *> postgresType)
      "table" -> PGTable <$> (ss *> char '(' *> cols `sepBy` (char ',') <* ss <* char ')')
      t       -> return $ PGSingle (unpack t)

  identifier = do
    s1 <- takeWhile1 (inClass "a-zA-Z_")
    s2 <- takeWhile (inClass "a-zA-Z_0-9$")
    return $ toLower $ s1 `append` s2

  postgresType = toLower <$> (foldr1 (<|>) $
       (map asciiCI [ "double precision" ])
    ++ (map (\t -> (asciiCI t <* ss <* (modifiers $ Just 1))) ["bit", "character varying"])
    ++ (map (\t -> (asciiCI t <* ss <* (modifiers $ Just 2))) ["numeric", "decimal"])
    ++ (map timeType ["timestamp", "time"])
    ++ [intervalType, identifier <* (modifiers Nothing)])

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
