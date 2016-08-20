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
    unwrapRow
  , unwrapColumn
  , PostgresBindOptions(..)
  ) where

import Data.Default (Default, def)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..))


-- | Options that specify how to construct the function binding.
data PostgresBindOptions = PostgresBindOptions
  { pboFunctionName :: PGFunction -> String
    -- ^ Function that generates name of a binding
  }

instance Default PostgresBindOptions where
  def = PostgresBindOptions
    { pboFunctionName = \(PGFunction _schema name _args _result) -> name }

-- | Remove 'Only' constructor.
unwrapColumn :: [Only a] -> [a]
unwrapColumn = map (\(Only x) -> x)

-- | Remove list and 'Only' constructors.
unwrapRow :: [Only a] -> a
unwrapRow = head . unwrapColumn

