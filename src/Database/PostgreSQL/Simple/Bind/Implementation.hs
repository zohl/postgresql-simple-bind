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

import Control.Arrow ((&&&), second)
import Control.Exception (throw)
import Control.Monad.Catch (MonadThrow, throwM)
import Debug.Trace (traceIO)
import Data.List (intersperse)
import Data.Monoid ((<>), mconcat)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGResult(..), PGColumn(..))
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


-- | Example: "varchar" -> PostgresType "varchar"
postgresT :: String -> Type
postgresT t = AppT (ConT ''PostgresType) (LitT (StrTyLit t))

-- | Example: ''FromField "varchar" a -> [PostgresType "varchar" ~ a, FromField a]
mkContextT :: Name -> String -> Name -> [Type]
mkContextT constraint typelit name = [
    EqualityT `AppT` (postgresT typelit) `AppT` (VarT name)
  , (ConT constraint) `AppT` VarT name
  ]

mkResultColumnT :: Name -> String -> Q (Name, [Type])
mkResultColumnT c t = (id &&& (mkContextT c t)) <$> newName "y"

mkReturnClauseT :: Bool -> [Type] -> Type
mkReturnClauseT isMultiple = (if isMultiple then (AppT ListT) else id) . \case
  [t] -> t
  ts  -> foldl AppT (TupleT (length ts)) (ts)

mkResultT' :: (t -> Q (Name, [Type])) -> ([Name] -> Type) -> [t] -> Q ([Name], [Type], Type)
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
mkResultT :: PostgresBindOptions -> String -> PGResult -> Q ([Name], [Type], Type)

mkResultT _opt _fname (PGSingle ts) = mkResultT' mkResultColumnT' mkReturnClauseT' ts where
  mkResultColumnT' = mkResultColumnT ''FromField
  mkReturnClauseT' = mkReturnClauseT False . map VarT

mkResultT  opt _fname (PGSetOf ts)  = mkResultT' mkResultColumnT' mkReturnClauseT' ts where
  mkResultColumnT' = (uncurry mkResultColumnT) . ((mkConstraintT . (pboSetOfReturnType opt)) &&& id)
  mkReturnClauseT' = mkReturnClauseT True . map VarT
  mkConstraintT = \case
    AsRow   -> ''FromRow
    AsField -> ''FromField

mkResultT opt fname (PGTable cols) = mkResultT' mkResultColumnT' mkReturnClauseT' cols where
  mkResultColumnT' = mkResultColumnT ''FromField . pgcType
  mkReturnClauseT' = mkReturnClauseT True . zipWith wrapColumn cols
  wrapColumn c t = if (pboIsNullable opt fname . pgcName $ c)
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
mkArgsT :: [PGArgument] -> Q ([Name], [Type], [Type])
mkArgsT cs = do
  names <- sequence $ replicate (length cs) (newName "x")
  let context = concat $ zipWith (\PGArgument {..} n -> mkContextT ''ToField pgaType n) cs names

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
  (argNames, argContext, argClause) <- mkArgsT pgfArguments
  (retNames, retContext, retClause) <- mkResultT opt pgfName pgfResult

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
unwrapE _   (PGSingle _)    q = (VarE 'fmap) `AppE` (VarE 'unwrapRow) `AppE` q
-- unwrapE opt (PGSetOf tname) q = unwrapE' (pboSetOfReturnType opt tname) q
-- unwrapE _   (PGTable _)     q = unwrapE' AsRow q


wrapArg :: PostgresBindOptions -> PGArgument -> Name -> Exp
wrapArg PostgresBindOptions {..} PGArgument {..} argName = foldl1 AppE $ [
    ConE $ if pgaOptional then 'OptionalArg else 'MandatoryArg
  , LitE $ StringL $ maybe "(N/A)" id pgaName
  , LitE $ StringL pgaType
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

  let queryBindings = BindS
        (TupP [VarP sqlQueryName, VarP refinedArgsName])
        ((VarE 'prepareQuery)
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
queryPrefix PostgresBindOptions {..} PGFunction {..} = select ++ " " ++ function where
  select = case pgfResult of
    PGTable _     -> mkSelect AsRow
--  PGSetOf tname -> mkSelect $ pboSetOfReturnType tname
    _             -> mkSelect AsField

  mkSelect AsRow   = "select * from"
  mkSelect AsField = "select"

  function = maybe
    pgfName
    (++ ("." ++ pgfName))
    pgfSchema

-- TODO:
-- - type casting
-- - (=>) (:=)
-- - throwing exception
runFormatter :: (MonadThrow m) => Bool -> [(PGArgument, Argument)] -> m [(Builder, Argument)]
runFormatter _ [] = return []
runFormatter o ((PGArgument {..}, arg):rest) = case arg of
  MandatoryArg {..} -> ((char8 '?', arg):) <$> (runFormatter o rest)
  OptionalArg  {..} -> case morgValue of
    Nothing  -> runFormatter True rest
    (Just _) -> case pgaName of
      Nothing -> if o
        then throwM (IncorrectInvocation "") -- . BSC8.unpack . BS.toStrict . toLazyByteString $ prefix)
        else ((char8 '?', arg):) <$> (runFormatter o rest)
      (Just name) -> (((byteString . BSC8.pack $ name) <> byteString " => ?", arg):) <$> (runFormatter o rest)


prepareQuery :: (MonadThrow m) => Builder -> [PGArgument] -> [Argument] -> m (Query, [Argument])
prepareQuery prefix specs args = do
  (argsString, refinedArgs) <- unzip <$> runFormatter False (zip specs args)
  let sqlQuery = mconcat [
          prefix, char8 '(', mconcat . intersperse (char8 ',') $ argsString, char8 ')']
  return (Query . BS.toStrict . toLazyByteString $ sqlQuery, refinedArgs)
