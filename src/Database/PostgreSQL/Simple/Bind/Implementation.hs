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
  Module:      Database.PostgreSQL.Simple.Bind.Implementation
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  TH functions that generate bindings. Examples are provided as
  pseudo-code snippets.
-}


module Database.PostgreSQL.Simple.Bind.Implementation (
    bindFunction
  , PostgresType
  ) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&), second)
import Control.Exception (throw)
import Control.Monad.Catch (MonadThrow, throwM)
import Debug.Trace (traceIO)
import Data.List (intersperse)
import Data.Monoid ((<>), mconcat)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGResult(..), PGColumn(..), PGIdentifier(..), PGType(..))
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..), PostgresBindOptions(..), ReturnType(..), unwrapRow, unwrapColumn)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..))
import Database.PostgreSQL.Simple.Types (Query(..))
import GHC.TypeLits (Symbol)
import Language.Haskell.TH.Syntax (Q, Dec(..), Exp(..), Type(..), Clause(..), Body(..), Pat(..))
import Language.Haskell.TH.Syntax (Name, mkName, newName, Lit(..), TyLit(..), TyVarBndr(..))
import Language.Haskell.TH.Syntax (Stmt(..), lift)
import qualified Data.ByteString.Char8 as BSC8

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BS

-- | Mapping from PostgreSQL types to Haskell types.
type family PostgresType (a :: Symbol)

-- | Create binding from given representation of a PostgreSQL function.
bindFunction :: PostgresBindOptions -> PGFunction -> Q [Dec]
bindFunction opt f = sequence $ (($ f) . ($ opt)) <$> [mkFunctionT, mkFunctionE]


data Argument
  = forall a . (ToField a) => MandatoryArg {
        argName           :: String
      , argType           :: String
      , argRepresentation :: Maybe String
      , margValue         :: a
      }
  | forall a . (ToField a) => OptionalArg {
        argName           :: String
      , argType           :: String
      , argRepresentation :: Maybe String
      , morgValue         :: Maybe a
      }

instance ToField Argument where
  toField MandatoryArg {..} = toField margValue
  toField OptionalArg {..} = maybe
    (throw . DefaultValueNotFound $ argName)
    toField
    morgValue

instance Show Argument where
  show arg = concat $ [
      argName arg
    , " => "
    , maybe (throw . RepresentationNotFound $ argName arg) id (argRepresentation arg)
    , " :: "
    , argType arg
    ]


-- | Example: "varchar" -> PostgresType "varchar"
postgresT :: String -> Type
postgresT t = AppT (ConT ''PostgresType) (LitT (StrTyLit t))

-- | Example: ''FromField "varchar" a -> [PostgresType "varchar" ~ a, FromField a]
mkContextT :: PostgresBindOptions -> Name -> PGType -> Name -> [Type]
mkContextT opt constraint t name = [
    EqualityT `AppT` (postgresT (formatType' opt t)) `AppT` (VarT name)
  , (ConT constraint) `AppT` VarT name
  ]

mkConstraintT :: ReturnType -> Name
mkConstraintT AsRow   = ''FromRow
mkConstraintT AsField = ''FromField

mkResultColumnT :: PostgresBindOptions -> Name -> PGType -> Q (Name, [Type])
mkResultColumnT opt c t = (id &&& (mkContextT opt c t)) <$> newName "y"

mkReturnClauseT :: Bool -> [Type] -> Type
mkReturnClauseT isMultiple = (if isMultiple then (AppT ListT) else id) . \case
  [t] -> t
  ts  -> foldl AppT (TupleT (length ts)) (ts)

mkResultT'
  :: (t -> Q (Name, [Type]))  -- ^ Generator of a new variable with necessary constraints.
  -> ([Name] -> Type)         -- ^ Generator of a return statement.
  -> [t]                      -- ^ Variables metadata (names+types or just types).
  -> Q ([Name], [Type], Type)
mkResultT' mkResultColumnT' mkReturnClauseT' ts = do
  (ns, cs) <- fmap (second concat . unzip) $ (mapM mkResultColumnT' ts)
  let clause = mkReturnClauseT' ns
  return (ns, cs, clause)

-- | Examples:
--     (PGSingle ["varchar"]) -> (["y"], [PostgresType "varchar" ~ y, FromField y], y)
--     (PGSingle ["varchar", "bigint"]) -> (
--         ["y", "z"]
--       , [PostgresType "varchar" ~ y, FromField y, PostgresType "bigint" ~ z, FromField z]
--       , (y, z))
--     (PGSetOf ["varchar"]) -> (["y"], [PostgresType "varchar" ~ y, FromField y], [y])
--     (PGTable ["bigint", "varchar"])  -> (
--         ["y", "z"]
--       , [PostgresType "bigint" ~ y, FromField y, PostgresType "varchar" ~ z, FromField z]
--       , (y, z))
mkResultT :: PostgresBindOptions -> PGIdentifier -> PGResult -> Q ([Name], [Type], Type)

mkResultT opt _fid (PGSingle ts) = mkResultT' mkResultColumnT' mkReturnClauseT' ts where
  mkResultColumnT' = mkResultColumnT opt ''FromField
  mkReturnClauseT' = mkReturnClauseT False . map VarT

mkResultT opt _fid (PGSetOf ts)  = mkResultT' mkResultColumnT' mkReturnClauseT' ts where
  mkResultColumnT' = mkResultColumnT opt
    (case ts of
        [t] -> mkConstraintT . pboSetOfReturnType opt $ t
        _   -> ''FromField)
  mkReturnClauseT' = mkReturnClauseT True . map VarT

mkResultT opt fid (PGTable cols) = mkResultT' mkResultColumnT' mkReturnClauseT' cols where
  mkResultColumnT' = mkResultColumnT opt ''FromField . pgcType
  mkReturnClauseT' = mkReturnClauseT True . zipWith wrapColumn cols
  wrapColumn c t = if (pboIsNullable opt fid . pgcName $ c)
                   then (ConT ''Maybe) `AppT` (VarT t)
                   else VarT t


-- | Example: [
--     PGArgument { pgaName = "x", pgaType = "varchar", pgaOptional = True }
--   , PGArgument { pgaName = "y", pgaType = "bigint", pgaOptional = False }
--   ] -> (
--     ["x1", "x2"]
--   , [PostgresType "varchar" ~ x1, ToField x1, PostgresType "bigint" ~ x2, ToField x2]
--   , [x1, Maybe x2]
--   )
mkArgsT :: PostgresBindOptions -> [PGArgument] -> Q ([Name], [Type], [Type])
mkArgsT opt cs = do
  names <- sequence $ replicate (length cs) (newName "x")
  let context = concat $ zipWith (\PGArgument {..} n -> mkContextT opt ''ToField pgaType n) cs names

  let defWrap d = case d of
        True  -> AppT (ConT ''Maybe)
        False -> id

  let clause = zipWith (\PGArgument {..} -> (defWrap pgaOptional) . VarT) cs names
  return (names, context, clause)


-- | Example: PGFunction {
--     pgfSchema    = "public"
--   , pgfName      = "foo"
--   , pgfResult    = PGSingle "varchar"
--   , pgfArguments = [
--       PGArgument { pgaName = "x", pgaType = "varchar", pgaOptional = True }
--     , PGArgument { pgaName = "y", pgaType = "bigint", pgaOptional = False }
--     ]
--   } -> {
--     foo :: forall x1 x2 x3. (
--          PostgresType "varchar" ~ x1, ToField x1
--        , PostgresType "bigint" ~ x2, ToField x2
--        , PostgresType "varchar" ~ x2, FromField x3
--        ) => Connection -> x1 -> Maybe x2 -> x3
--  }
mkFunctionT :: PostgresBindOptions -> PGFunction -> Q Dec
mkFunctionT opt@(PostgresBindOptions {..}) f@(PGFunction {..}) = do
  (argNames, argContext, argClause) <- mkArgsT opt pgfArguments
  (retNames, retContext, retClause) <- mkResultT opt pgfIdentifier pgfResult

  let vars = map PlainTV (argNames ++ retNames)
  let context = argContext ++ retContext

  let chain x = AppT (AppT ArrowT x)
  let clause = foldr1 chain $ (ConT ''Connection):(argClause ++ [AppT (ConT ''IO) retClause])

  return $ SigD (mkName $ pboFunctionName f) $ ForallT vars context clause


unwrapE' :: ReturnType -> Exp -> Exp
unwrapE' AsRow   q = q
unwrapE' AsField q = (VarE 'fmap) `AppE` (VarE 'unwrapColumn) `AppE` q

-- | Examples:
--     (PGSingle _) q -> { unwrapRow  q }
--     (PGSetOf  _) q -> { unwrapColumn q }
--     (PGTable  _) q -> { q }
unwrapE :: PostgresBindOptions -> PGResult -> Exp -> Exp
unwrapE _   (PGSingle _)  q = (VarE 'fmap) `AppE` (VarE 'unwrapRow) `AppE` q
unwrapE opt (PGSetOf [t]) q = unwrapE' (pboSetOfReturnType opt t) q
unwrapE _   (PGSetOf   _) q = unwrapE' AsRow q
unwrapE _   (PGTable _)   q = unwrapE' AsRow q


wrapArg :: PostgresBindOptions -> PGArgument -> Name -> Exp
wrapArg opt@(PostgresBindOptions {..}) PGArgument {..} argName = foldl1 AppE $ [
    ConE $ if pgaOptional then 'OptionalArg else 'MandatoryArg
  , LitE $ StringL $ maybe "(N/A)" id pgaName
  , LitE $ StringL (formatType' opt pgaType)
  , if pboDebugQueries
      then foldr1 AppE [ConE 'Just, VarE 'show, VarE argName]
      else ConE 'Nothing
  , VarE argName]


traceE :: Exp -> Exp
traceE e = (VarE 'traceIO) `AppE` ((VarE 'show) `AppE` e)


-- | Example: PGFunction {
--     pgfSchema    = "public"
--   , pgfName      = "foo"
--   , pgfResult    = PGSingle "varchar"
--   , pgfArguments = [
--       PGArgument { pgaName = "x", pgaType = "varchar", pgaOptional = True }
--     , PGArgument { pgaName = "y", pgaType = "bigint", pgaOptional = False }
--     ]
--   } -> {
--     foo conn x1 x2 = query conn
--       (Query $ BS.pack $ concat ["select public.foo (", (formatArguments args), ")"])
--       (filterArguments [MandatoryArg "x1" x1, OptionalArg "x2" x2])
--   }
mkFunctionE :: PostgresBindOptions -> PGFunction -> Q Dec
mkFunctionE opt@(PostgresBindOptions {..}) f@(PGFunction {..}) = do

  let funcName = mkName (pboFunctionName f)

  varNames <- sequence $ replicate (length pgfArguments) (newName "x")
  connName <- newName "conn"
  let funcArgs = (VarP connName):(map VarP varNames)

  [argsName, sqlQueryName, refinedArgsName] <- mapM newName ["args", "sqlQuery", "refinedArgs"]
  specs  <- lift pgfArguments

  let funcDecl = [
          ValD (VarP argsName)
          (NormalB . ListE $ zipWith (wrapArg opt) pgfArguments varNames) []]

  let traceQuery = NoBindS
        <$> if pboDebugQueries
            then (traceE $ VarE sqlQueryName):
                 (if null varNames then [] else [traceE $ VarE argsName])
            else []

  explicitCasts   <- lift pboExplicitCasts
  olderCallSyntax <- lift pboOlderCallSyntax
  defaultSchema   <- lift pboDefaultSchema
  let queryBindings = BindS
        (TupP [VarP sqlQueryName, VarP refinedArgsName])
        ((VarE 'prepareQuery)
          `AppE` explicitCasts
          `AppE` olderCallSyntax
          `AppE` defaultSchema
          `AppE` (LitE . StringL $ queryPrefix opt f)
          `AppE` specs
          `AppE` (VarE argsName))

  let execQuery = NoBindS $ unwrapE opt pgfResult $ CondE ((VarE 'null) `AppE` (VarE refinedArgsName))
        ((VarE 'query_)
          `AppE` (VarE connName)
          `AppE` (VarE sqlQueryName))
        ((VarE 'query)
          `AppE` (VarE connName)
          `AppE` (VarE sqlQueryName)
          `AppE` (VarE refinedArgsName))

  let funcBody = NormalB . DoE $ queryBindings:(concat [traceQuery, [execQuery]])

  return $ FunD funcName [Clause funcArgs funcBody funcDecl]



queryPrefix :: PostgresBindOptions -> PGFunction -> String
queryPrefix opt@(PostgresBindOptions {..}) PGFunction {..} =
  select ++ " " ++ (formatIdentifier' opt pgfIdentifier) where

    select = case pgfResult of
      PGTable _     -> mkSelect AsRow
      PGSetOf [t]   -> mkSelect . pboSetOfReturnType $ t
      PGSetOf _     -> mkSelect AsRow
      _             -> mkSelect AsField

    mkSelect AsRow   = "select * from"
    mkSelect AsField = "select"



-- untypedPlaceholder, typedPlaceholder :: String -> String
-- untypedPlaceholder = const "?"
-- typedPlaceholder atype = "(?)::" ++ atype


-- formatArgument :: String -> (String -> String) -> Argument -> Maybe String
-- formatArgument callSyntax placeholder = format where
--   format = \case
--     MandatoryArg {..} -> Just $ placeholder argType
--     OptionalArg {..} -> fmap
--       (const . concat $ [argName, callSyntax, placeholder argType])
--       morgValue
--
--
-- formatArguments :: String -> (String -> String) -> [Argument] -> String
-- formatArguments callSyntax placeholder = concat
--   . (intersperse ", ")
--   . catMaybes
--   . (map $ formatArgument callSyntax placeholder)
--
--
-- filterArguments :: [Argument] -> [Argument]
-- filterArguments = filter isPresented where
--   isPresented :: Argument -> Bool
--   isPresented (OptionalArg {..}) = maybe False (const True) $ morgValue
--   isPresented _                  = True


formatIdentifier' :: PostgresBindOptions -> PGIdentifier -> String
formatIdentifier' PostgresBindOptions {..} PGIdentifier {..} = maybe
  pgiName
  (++ ("." ++ pgiName))
  (pgiSchema <|> pboDefaultSchema)

formatType' :: PostgresBindOptions -> PGType -> String
formatType' opt PGType {..} = formatIdentifier' opt pgtIdentifier


formatIdentifier :: Maybe String -> PGIdentifier -> Builder
formatIdentifier defaultSchema PGIdentifier {..} = byteString . BSC8.pack $ maybe
  pgiName
  (++ ("." ++ pgiName))
  (pgiSchema <|> defaultSchema)

formatType :: Maybe String -> PGType -> Builder
formatType defaultSchema PGType {..} = formatIdentifier defaultSchema $ pgtIdentifier

formatArgument :: Bool -> Maybe String -> PGArgument -> Builder
formatArgument True  ds PGArgument {..} = byteString "(?)::" <> (formatType ds pgaType)
formatArgument False _  _               = char8 '?'

formatAssingment :: Bool -> String -> Builder
formatAssingment ocs name = (byteString . BSC8.pack $ name) <> byteString (if ocs then ":=" else "=>")

runFormatter
  :: Bool                     -- ^ Explicit casts.
  -> Bool                     -- ^ Older call syntax.
  -> Maybe String             -- ^ Default schema.
  -> Bool                     -- ^ Was optional argument omitted?
  -> [(PGArgument, Argument)] -- ^ Arguments with metadata.
  -> Maybe [(Builder, Argument)]
runFormatter _ _ _ _ [] = return []
runFormatter ec ocs ds o ((pga, arg):rest) = case arg of
  MandatoryArg {..} -> ((formatArgument ec ds pga, arg):) <$> (runFormatter ec ocs ds o rest)
  OptionalArg  {..} -> case morgValue of
    Nothing  -> runFormatter ec ocs ds True rest
    (Just _) -> case (pgaName pga) of
      Nothing -> if o
        then Nothing
        else ((char8 '?', arg):) <$> (runFormatter ec ocs ds o rest)
      (Just name) -> ((formatAssingment ocs name <> (formatArgument ec ds pga), arg):)
                 <$> (runFormatter ec ocs ds o rest)

prepareQuery :: (MonadThrow m)
  => Bool          -- ^ Explicit casts.
  -> Bool          -- ^ Older call syntax.
  -> Maybe String  -- ^ Default schema.
  -> Builder       -- ^ Query prefix.
  -> [PGArgument]  -- ^ Arguments to format.
  -> [Argument]
  -> m (Query, [Argument])
prepareQuery ec ocs ds prefix specs args = do
  (argsString, refinedArgs) <- maybe
    (throwM (IncorrectInvocation . show $ (specs, args)))
    (return)
    (unzip <$> runFormatter ec ocs ds False (zip specs args))
  let sqlQuery = mconcat [
          prefix, char8 '(', mconcat . intersperse (char8 ',') $ argsString, char8 ')']
  return (Query . BS.toStrict . toLazyByteString $ sqlQuery, refinedArgs)
