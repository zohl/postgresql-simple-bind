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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


module Database.PostgreSQL.Simple.Bind.Representation (
    PGFunction(..)
  , PGArgumentClass(..)
  , PGArgument(..)
  , PGArgumentMode(..)
  , PGColumn(..)
  , PGResultClass(..)
  , PGResult(..)
  , PGIdentifier(..)
  , PGTypeClass(..)
  , PGType(..)
  ) where


import Data.Default (Default, def)
import Data.String (IsString(..))
import Language.Haskell.TH.Syntax (Lift)

-- | Representation of a function argument's mode.
data PGArgumentMode = In | Out | InOut | Variadic deriving (Show, Eq, Lift, Enum, Bounded)

instance Default PGArgumentMode where
  def = In

-- | Representation of a qualified identificator.
data PGIdentifier = PGIdentifier {
    pgiSchema :: Maybe String
  , pgiName   :: String
  } deriving (Show, Eq, Lift)

instance IsString PGIdentifier where
  fromString s = PGIdentifier { pgiSchema = Nothing, pgiName = s }


-- | Class of types that can represent a PostgreSQL type.
class (Show t, Eq t) => PGTypeClass t where
  mergeTypes :: [t] -> [t] -> Maybe [t]

-- | Representation of a PostgreSQL type.
data PGType = PGType {
    pgtIdentifier :: PGIdentifier
  , pgtModifiers  :: Maybe String
  } deriving (Show, Eq, Lift)

instance IsString PGType where
  fromString s = PGType { pgtIdentifier = fromString s, pgtModifiers = Nothing }

instance PGTypeClass PGType where
  mergeTypes ts ts' =
    if ts == ts' || ((length ts' > 1) && ts == [recordType])
    then Just ts'
    else Nothing where
      recordType = PGType {
          pgtIdentifier = PGIdentifier {pgiSchema = Nothing, pgiName = "record"}
        , pgtModifiers = Nothing
        }


-- | Class of types that can represent an argument of PostgreSQL function.
class (PGTypeClass t, Show a, Eq a) => PGArgumentClass a t | a -> t where
  argumentMode     :: a -> PGArgumentMode
  argumentOptional :: a -> Bool
  argumentType     :: a -> t

-- | Representation of a function's argument.
data PGArgument = PGArgument {
    pgaMode     :: PGArgumentMode
  , pgaName     :: Maybe String
  , pgaType     :: PGType
  , pgaOptional :: Bool
  } deriving (Show, Eq, Lift)

instance PGArgumentClass PGArgument PGType where
  argumentMode     = pgaMode
  argumentOptional = pgaOptional
  argumentType     = pgaType


-- | Representation of a PostrgeSQL function signature (schema, name, arguments, result).
data PGFunction = PGFunction {
    pgfIdentifier :: PGIdentifier
  , pgfArguments  :: [PGArgument]
  , pgfResult     :: PGResult
  } deriving (Show, Eq)

-- | Representation of a resultant's column (name, type).
data PGColumn = PGColumn {
    pgcName :: String
  , pgcType :: PGType
  } deriving (Show, Eq)


-- | Class of types that can represent a result of PostgreSQL function.
class (PGTypeClass t, Show r, Eq r) => PGResultClass r t | r -> t where
  mergeResults :: r -> r -> Maybe r
  resultSingle :: [t] -> r

-- | Representation of a function's return value.
data PGResult
  = PGSingle [PGType]
  | PGSetOf  [PGType]
  | PGTable  [PGColumn]
    deriving (Show, Eq)

instance PGResultClass PGResult PGType where
  mergeResults (PGSingle ts) (PGSingle ts') = PGSingle <$> (mergeTypes ts ts')
  mergeResults (PGSetOf  ts) (PGSetOf  ts') = PGSetOf  <$> (mergeTypes ts ts')
  mergeResults (PGSetOf  ts) (PGSingle ts') = PGSetOf  <$> (mergeTypes ts ts')
  mergeResults _             _              = Nothing

  resultSingle = PGSingle
