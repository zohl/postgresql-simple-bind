{-|
  Module:      Database.PostgreSQL.Simple.Bind
  Copyright:   (c) 2016 Al Zohali
  License:     GPL3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental


  = Description
  
  This extension of "Database.PostgreSQL.Simple" faciliates to create bindings
  for PostgreSQL stored functions. By binding we mean a haskell function that:

  * takes 'Database.PostgreSQL.Simple.Types.Connection' argument

  * takes arguments required by the stored function

  * performs call to database and returns result
  
  To get working haskell function the one must provide only a
  stored function's signature, while all the grunt work like explicit type
  specifications and unpacking the results are done behind the scenes.
  This allows us to implement a desing pattern where an application communicates
  with a database via API and does not have to be aware of it's internal structure.

-}

module Database.PostgreSQL.Simple.Bind (
    bindFunction
  , Options(..)
  , defaultOptions
  , PostgresType
  ) where


import Database.PostgreSQL.Simple.Bind.Implementation
import Database.PostgreSQL.Simple.Bind.Util (Options (..), defaultOptions)

