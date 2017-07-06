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
  , PGIdentifier(..)
  , PGType(..)
  , mergePGResults
  ) where


import Data.Default (Default, def)
import Data.String (IsString(..))
import Language.Haskell.TH.Syntax (Lift)

-- | Representation of a function argument's mode.
data PGArgumentMode = In | Out | InOut | Variadic deriving (Show, Eq, Lift)

instance Default PGArgumentMode where
  def = In

-- | Representation of a qualified identificator.
data PGIdentifier = PGIdentifier {
    pgiSchema :: Maybe String
  , pgiName   :: String
  } deriving (Show, Eq, Lift)

instance IsString PGIdentifier where
  fromString s = PGIdentifier { pgiSchema = Nothing, pgiName = s }


data PGType = PGType {
    pgtIdentifier :: PGIdentifier
  , pgtModifiers  :: Maybe String
  } deriving (Show, Eq, Lift)

instance IsString PGType where
  fromString s = PGType { pgtIdentifier = fromString s, pgtModifiers = Nothing }


-- | Representation of a function's argument.
data PGArgument = PGArgument {
    pgaMode     :: PGArgumentMode
  , pgaName     :: Maybe String
  , pgaType     :: PGType
  , pgaOptional :: Bool
  } deriving (Show, Eq, Lift)

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

-- | Representation of a function's return value.
data PGResult
  = PGSingle [PGType]
  | PGSetOf  [PGType]
  | PGTable  [PGColumn]
    deriving (Show, Eq)


mergeTypes :: [PGType] -> [PGType] -> Maybe [PGType]
mergeTypes ts ts' =
  if ts == ts' || ((length ts' > 1) && ts == [recordType])
  then Just ts'
  else Nothing where
    recordType = PGType {
        pgtIdentifier = PGIdentifier {pgiSchema = Nothing, pgiName = "record"}
      , pgtModifiers = Nothing
      }

-- | Merge 'PGResult' derived from RETURNING clause (first) and
--   derived from arguments (second).
mergePGResults :: PGResult -> PGResult -> Maybe PGResult
mergePGResults (PGSingle ts) (PGSingle ts') = PGSingle <$> (mergeTypes ts ts')
mergePGResults (PGSetOf  ts) (PGSetOf  ts') = PGSetOf  <$> (mergeTypes ts ts')
mergePGResults (PGSetOf  ts) (PGSingle ts') = PGSetOf  <$> (mergeTypes ts ts')
mergePGResults _             _              = Nothing
