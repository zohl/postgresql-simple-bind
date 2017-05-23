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
  Module:      Database.PostgreSQL.Simple.Bind.Common
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  Common functions and types.
-}


module Database.PostgreSQL.Simple.Bind.Common (
    PostgresBindOptions(..)
  , PostgresBindException(..)
  , ReturnType(..)
  , unwrapColumn
  , unwrapRow
  ) where

import Control.Monad.Catch (Exception)
import Data.Default (Default, def)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..))
import Data.Typeable (Typeable)

-- | The exception is thrown when something goes wrong with this package.
data PostgresBindException
  = ParserFailed String
    -- ^ Thrown when parser fails to process a function
    -- declaration. Arguments of the constructor: error message from the parser.
  | DefaultValueNotFound String
    -- ^ Thrown when 'Argument' expected to have default value while
    -- it doesn't. Actually this should never happen, but we all know...
  | RepresentationNotFound String
    -- ^ Thrown when 'Argument' is being printed, but representation was't
    -- provided. Again, this should never happen.
  | IncorrectInvocation String
    -- ^ Thrown when function is called with wrong combination of
    -- non-default values. More precisely, supplying default value to named
    -- argument and then supplying non-default value to positional argument
    -- is not allowed.
  deriving (Eq, Show, Typeable)

instance Exception PostgresBindException


-- | How to interpret results of function execution.
data ReturnType
    = AsRow
    | AsField

-- | Options that specify how to construct the function binding.
data PostgresBindOptions = PostgresBindOptions {
    pboFunctionName    :: PGFunction -> String
    -- ^ Function that generates name of a binding.
  , pboIsNullable      :: String -> String -> Bool
    -- ^ Which columns in returned tables can be null.
  , pboSetOfReturnType :: String -> ReturnType
    -- ^ How to process type in "setof" clause.
  , pboExplicitCasts   :: Bool
    -- ^ Whether to add explicit type casts to arguments.
  , pboOlderCallSyntax :: Bool
    -- ^ Whether to use old-style call syntax (:=) instead of (=>). This is
    --   necessary for PostgreSQL < 9.5.
  , pboDebugQueries    :: Bool
    -- ^ Whether to print executed queries and their arguments.
  , pboIgnoreFiles     :: FilePath -> Bool
    -- ^ Which files do not search for bindings.
  }

instance Default PostgresBindOptions where
  def = PostgresBindOptions {
      pboFunctionName    = pgfName
    , pboIsNullable      = const . const $ False
    , pboSetOfReturnType = const AsField
    , pboExplicitCasts   = True
    , pboOlderCallSyntax = True
    , pboDebugQueries    = False
    , pboIgnoreFiles     = const True
    }

-- | Remove 'Only' constructor.
unwrapColumn :: [Only a] -> [a]
unwrapColumn = map (\(Only x) -> x)

-- | Remove list and 'Only' constructors.
unwrapRow :: [Only a] -> a
unwrapRow = head . unwrapColumn
