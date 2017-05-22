{-|
  Module:      Database.PostgreSQL.Simple.Bind.Representation
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  An algebraic data types that (partially) represents function declaration based on
  <http://www.postgresql.org/docs/9.5/static/sql-createfunction.html PostgreSQL documentation>.
-}

{-# LANGUAGE DeriveLift #-}


module Database.PostgreSQL.Simple.Bind.Representation (
    PGFunction(..)
  , PGArgument(..)
  , PGArgumentMode(..)
  , PGColumn(..)
  , PGResult(..)
  ) where


import Data.Default (Default, def)
import Language.Haskell.TH.Syntax (Lift)

-- | Representation of a function argument's mode.
data PGArgumentMode = In | Out | InOut | Variadic deriving (Show, Eq, Lift)

instance Default PGArgumentMode where
  def = In


-- | Representation of a function's argument.
data PGArgument = PGArgument {
    pgaMode     :: PGArgumentMode
  , pgaName     :: Maybe String
  , pgaType     :: String
  , pgaOptional :: Bool
  } deriving (Show, Eq, Lift)

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
