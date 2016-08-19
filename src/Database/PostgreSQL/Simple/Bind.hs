{-|
  Module:      Database.PostgreSQL.Simple.Bind
  Copyright:   (c) 2016 Al Zohali
  License:     GPL3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental


  = Description

  postgresql-simple-bind is an extension for postgresql-simple library that
  faciliates and automates bindings creation. This is especially useful in a
  design pattern where an application communicates with a database via API hiding
  the internal structure of the latter.

-}

module Database.PostgreSQL.Simple.Bind (
    bindFunction

  , PostgresBindOptions(..)
  , PostgresType

  , PGFunction(..)
  , PGArgument(..)
  , PGColumn(..)
  , PGResult(..)
  ) where


import Database.PostgreSQL.Simple.Bind.Implementation
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGColumn(..), PGResult(..))
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindOptions(..))

