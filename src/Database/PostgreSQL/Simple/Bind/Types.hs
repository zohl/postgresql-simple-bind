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
  Module:      Database.PostgreSQL.Simple.Bind.Types
  Copyright:   (c) 2016 Al Zohali
  License:     GPL3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  Default instances for PostgresType family.
-}


module Database.PostgreSQL.Simple.Bind.Types (
    PostgresType
  ) where

import Data.Text
import Data.Time
import Database.PostgreSQL.Simple.Bind.Implementation (PostgresType)


type instance PostgresType "character varying" = Text
type instance PostgresType "varchar"           = Text

type instance PostgresType "bigint"            = Int

type instance PostgresType "void"              = ()

type instance PostgresType "timestamp with time zone" = UTCTime
type instance PostgresType "timestamptz"              = UTCTime

