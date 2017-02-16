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

  An algebraic data type that (partially) represents function declaration based on
  <http://www.postgresql.org/docs/9.5/static/sql-createfunction.html PostgreSQL documentation>.
-}

module Database.PostgreSQL.Simple.Bind.Representation (
    PGFunction(..)
  , PGArgument(..)
  , PGColumn(..)
  , PGResult(..)
  , PostgresBindException(..)
  , parsePGFunction
  ) where


import Control.Applicative ((*>), (<*), (<|>))
import Control.Monad.Catch (MonadThrow(..), Exception, throwM)
import Data.Attoparsec.Text (Parser, char, string, decimal, skipSpace, asciiCI, sepBy)
import Data.Attoparsec.Text (takeWhile, takeWhile1, parseOnly, takeTill, inClass)
import Data.Text (Text, toLower, unpack, append, length)
import Data.Typeable (Typeable)
import Prelude hiding (takeWhile, length)

-- | The exception is thrown when something goes wrong with this package.
data PostgresBindException
  = ParserFailed String
    -- ^ Thrown when parser fails to process a function
    -- declaration. Arguments of the constructor: error message from the parser.
  | DefaultValueNotFound String
    -- ^ Thrown when 'Argument' expected to have default value while
    -- it doesn't. Actually this should never happen, but we all know...
  | RepresentationNotFound String
    -- ^ Thrown when 'Argument' is beign printed, but representation was't
    -- provided. Again, this should never happen.
  deriving (Eq, Show, Typeable)

instance Exception PostgresBindException


-- | Representation of a function's argument.
data PGArgument = PGArgument {
    pgaName     :: String
  , pgaType     :: String
  , pgaOptional :: Bool
  } deriving (Show, Eq)

-- | Representation of a PostrgeSQL function signature (schema, name, arguments, result).
data PGFunction = PGFunction {
    pgfSchema    :: String
  , pgfName      :: String
  , pgfArguments :: [PGArgument]
  , pgfResult    :: PGResult
  } deriving (Show, Eq)

-- | Representation of a resultant's column (name, type).
data PGColumn = PGColumn {
    pgcName :: String
  , pgcType :: String
  } deriving (Show, Eq)

-- | Representation of a function's return value.
data PGResult
  = PGSingle String
  | PGSetOf  String
  | PGTable  [PGColumn]
    deriving (Show, Eq)

-- | Takes PostgreSQL function signature and represent it as an algebraic data type.
parsePGFunction :: (MonadThrow m) => Text -> m PGFunction
parsePGFunction s = either
  (\err -> throwM . ParserFailed . concat $ ["In declaration `", unpack s, "`: ", err])
  return
  (parseOnly (ss *> function) s) where
    ss = skipSpace

    function = do
      _            <- asciiCI "function"
      pgfSchema    <- fmap unpack $ ss *> ((identifier <* (char '.')) <|> (string ""))
      pgfName      <- fmap unpack $ ss *> identifier
      pgfArguments <- ss *> char '(' *> (arguments `sepBy` (char ','))
      pgfResult    <- ss *> char ')' *> returnType
      _            <- ss
      return PGFunction {..}

    arguments = do
      pgaName     <- fmap unpack $ ss *> identifier
      pgaType     <- fmap unpack $ ss *> postgresType
      pgaOptional <- fmap (( > 0) . length) $ ss *> (
              (asciiCI "default" <|> string "=") *> (takeTill (inClass ",)"))
          <|> (string ""))
      -- WARNING: parsing default values requires ability to parse almost arbitraty expressions.
      -- Here is a quick and dirty implementation of the parser.
      return PGArgument {..}

    cols = do
      pgcName <- fmap unpack $ ss *> identifier
      pgcType <- fmap unpack $ ss *> postgresType <* ss
      return PGColumn {..}

    returnType = ss *> asciiCI "returns" *> ss *> (fmap toLower $
          asciiCI "setof"
      <|> asciiCI "table"
      <|> postgresType) >>= \case
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
      ++ ((asciiCI "timestamptz"):(map timeType ["timestamp", "time"]))
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

