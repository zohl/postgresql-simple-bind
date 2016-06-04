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


import Prelude hiding (takeWhile, length)
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text                    (Text, toLower, pack, unpack, append, length)



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
parsePGFunction s = result where
  result = case (parseOnly parseFunction s) of
    Right x -> x

  ss = skipSpace

  parseFunction  = do
    ss *> asciiCI "function" 
    schema <- ss *> ((parseIdentifier <* (char '.')) <|> (string ""))
    name <- ss *> parseIdentifier
    args <- ss *> char '(' *> (parseArgs `sepBy` (char ','))
    ret  <- ss *> char ')' *> parseReturn
    return $ PGFunction (unpack schema) (unpack name) args ret

  parseArgs = do
    name     <- ss *> parseIdentifier
    datatype <- ss *> parseType
    def      <- ss *> ((asciiCI "default" <|> string "=") *> (takeTill (inClass ",)")) <|> (string ""))
    -- WARNING: parsing default values requires ability to parse almost arbitraty expressions.
    -- Here is a quick and dirty implementation of the parser.

    return $ PGArgument (unpack name) (unpack datatype) ((length def) > 0)
 
  parseCols = do
    name     <- ss *> parseIdentifier
    datatype <- ss *> parseType <* ss
    return $ PGColumn (unpack name) (unpack datatype)

  parseReturn = do
    ret <- ss *> asciiCI "returns" *> ss *> (
               asciiCI "setof"
           <|> asciiCI "table"
           <|> parseType)

    case toLower(ret) of
      "setof" -> (PGSetOf . unpack) <$> (ss *> parseType)
      "table" -> PGTable <$> (ss *> char '(' *> parseCols `sepBy` (char ',') <* ss <* char ')')
      t       -> return $ PGSingle (unpack t)

  parseIdentifier = do
    s1 <- takeWhile1 (inClass "a-zA-Z_")
    s2 <- takeWhile (inClass "a-zA-Z_0-9$")
    return $ toLower $ s1 `append` s2

  parseType = toLower <$> (foldr1 (<|>) $
       (map asciiCI [ "double precision" ])
    ++ (map (\s -> (asciiCI s <* ss <* (parseModifiers 1))) ["bit", "character varying"])
    ++ (map (\s -> (asciiCI s <* ss <* (parseModifiers 2))) ["numeric", "decimal"])
    ++ (map parseTime ["timestamp", "time"])
    ++ [parseInterval, parseIdentifier <* (parseModifiers 4)])
  -- WARNING: user defined types can have more complex modifiers.
  -- The argument of parseModifiers might be a subject to change.

  parseTime s = do
    base <- asciiCI s <* ss <* (parseModifiers 1)
    tz <- ss *> ((asciiCI "with time zone") <|> (asciiCI "without time zone") <|> (string ""))
    return $ case tz of
      "" -> base
      _  -> base `append` " " `append` tz
 
  parseInterval = do
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
    ss *> (parseModifiers 1)

    return $ case fields of
      "" -> base
      _  -> base `append` " " `append` fields
 

  parseModifiers n = ((char '(') *>(foldl (*>)
                                          (ss *> decimal *> ss)
                                          (replicate (n - 1) (char ',' *> ss *> decimal *> ss)))
                                *> (char ')') *> (string ""))
                 <|> (case n of
                        1 -> (string "")
                        _ -> (parseModifiers (n - 1)))
                        

