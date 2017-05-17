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

import Control.Exception (throw)
import Debug.Trace (traceIO)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
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
import Language.Haskell.TH.Syntax (Stmt(..))
import qualified Data.ByteString.Char8 as BS


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

untypedPlaceholder, typedPlaceholder :: String -> String
untypedPlaceholder = const "?"
typedPlaceholder atype = "(?)::" ++ atype


formatArgument :: String -> (String -> String) -> Argument -> Maybe String
formatArgument callSyntax placeholder = format where
  format = \case
    MandatoryArg {..} -> Just $ placeholder argType
    OptionalArg {..} -> fmap
      (const . concat $ [argName, callSyntax, placeholder argType])
      morgValue


formatArguments :: String -> (String -> String) -> [Argument] -> String
formatArguments callSyntax placeholder = concat
  . (intersperse ", ")
  . catMaybes
  . (map $ formatArgument callSyntax placeholder)


filterArguments :: [Argument] -> [Argument]
filterArguments = filter isPresented where
  isPresented :: Argument -> Bool
  isPresented (OptionalArg {..}) = maybe False (const True) $ morgValue
  isPresented _                  = True


-- | Example: "varchar" -> PostgresType "varchar"
postgresT :: String -> Type
postgresT t = AppT (ConT ''PostgresType) (LitT (StrTyLit t))

-- | Example: ''FromField "varchar" a -> [PostgresType "varchar" ~ a, FromField a]
mkContextT :: Name -> String -> Name -> [Type]
mkContextT constraint typelit name = [
    EqualityT `AppT` (postgresT typelit) `AppT` (VarT name)
  , (ConT constraint) `AppT` VarT name
  ]

-- | Examples:
--     (PGSingle "varchar") -> (["y"], [PostgresType "varchar" ~ y, FromField y], y)
--     (PGSetOf "varchar") -> (["y"], [PostgresType "varchar" ~ y, FromField y], [y])
--     (PGTable ["bigint", "varchar"])  -> (
--         ["y", "z"]
--       , [PostgresType "bigint" ~ y, FromField y, PostgresType "varchar" ~ z, FromField z]
--       , (y, z))
mkResultT :: PostgresBindOptions -> String -> PGResult -> Q ([Name], [Type], Type)
mkResultT _ _ (PGSingle t) = do
  name <- newName "y"
  return ([name], mkContextT ''FromField t name, VarT name)

mkResultT (PostgresBindOptions {..}) _fname (PGSetOf tname) = do
  name <- newName "y"
  let constraint = case (pboSetOfReturnType tname) of
        AsRow   -> ''FromRow
        AsField -> ''FromField
  return ([name], mkContextT constraint tname name, ListT `AppT` (VarT name))

mkResultT (PostgresBindOptions {..}) fname (PGTable cs) = do
  names <- sequence $ replicate (length cs) (newName "y")
  let context = concat $
        zipWith (\(PGColumn _ typelit) name -> mkContextT ''FromField typelit name) cs names

  let wrapColumn (PGColumn cname _ctype) = case pboIsNullable fname cname of
        True  -> AppT (ConT ''Maybe)
        False -> id

  let clause = AppT ListT $ foldl AppT (TupleT (length cs)) $
        zipWith wrapColumn cs (map VarT names)

  return (names, context, clause)


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


-- | Example: PGFunction {
--     pgfSchema    = "public"
--   , pgfName      = "foo"
--   , pgfResult    = PGSingle "varchar"
--   , pgfArguments = [
--       PGArgument { pgaName = "x", pgaType = "varchar", pgaOptional = True }
--     , PGArgument { pgaName = "y", pgaType = "bigint", pgaOptional = False }
--     ]
--   } -> {
--     Query $ BS.pack $ concat ["select public.foo (", (formatArguments args), ")"]
--   }
mkSqlQuery :: PostgresBindOptions -> PGFunction -> Maybe Name -> Exp
mkSqlQuery PostgresBindOptions {..} PGFunction {..} argsName =
  toQuery . AppE (VarE 'concat) . ListE $ [
      mkStrLit $ concat [prefix, " ", functionName, "("]
    , maybe (mkStrLit "") (\args -> foldl1 AppE [
        VarE 'formatArguments
      , mkStrLit $ if pboOlderCallSyntax then " := " else " => "
      , VarE $ if pboExplicitCasts then 'typedPlaceholder else 'untypedPlaceholder
      , VarE args
      ]) argsName
    , mkStrLit ")"] where

  prefix = case pgfResult of
    PGTable _     -> mkSelect AsRow
    PGSetOf tname -> mkSelect $ pboSetOfReturnType tname
    _             -> mkSelect AsField

  mkSelect AsRow   = "select * from"
  mkSelect AsField = "select"

  functionName = case pgfSchema of
    "" -> pgfName
    _  -> pgfSchema ++ "." ++ pgfName

  mkStrLit s = LitE (StringL s)
  toQuery = AppE (ConE 'Query) . AppE (VarE 'BS.pack)


unwrapE' :: ReturnType -> Exp -> Exp
unwrapE' AsRow   q = q
unwrapE' AsField q = (VarE 'fmap) `AppE` (VarE 'unwrapColumn) `AppE` q

-- | Examples:
--     (PGSingle _) q -> { unwrapRow  q }
--     (PGSetOf  _) q -> { unwrapColumn q }
--     (PGTable  _) q -> { q }
unwrapE :: PostgresBindOptions -> PGResult -> Exp -> Exp
unwrapE _   (PGSingle _)    q = (VarE 'fmap) `AppE` (VarE 'unwrapRow) `AppE` q
unwrapE opt (PGSetOf tname) q = unwrapE' (pboSetOfReturnType opt tname) q
unwrapE _   (PGTable _)     q = unwrapE' AsRow q


wrapArg :: PostgresBindOptions -> PGArgument -> Name -> Exp
wrapArg (PostgresBindOptions {..}) (PGArgument n t d) argName = foldl1 AppE $ [
    ConE $ if d then 'OptionalArg else 'MandatoryArg
  , LitE $ StringL n
  , LitE $ StringL t
  , if pboDebugQueries
      then foldr1 AppE [ConE 'Just, VarE 'show, VarE argName]
      else ConE 'Nothing
  , VarE argName]


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
  names <- sequence $ replicate (length pgfArguments) (newName "x")
  connName <- newName "conn"

  argsName <- case (null pgfArguments) of
    True  -> return Nothing
    False -> Just <$> newName "args"

  let argsExpr = AppE
        (VarE 'filterArguments)
        (ListE $ zipWith (wrapArg opt) pgfArguments names)

  sqlQueryName <- newName "sqlQuery"
  let sqlQueryExpr = mkSqlQuery opt f argsName

  let funcName = mkName $ pboFunctionName f
  let funcArgs = (VarP connName):(map VarP names)

  let funcBody = NormalB $ if pboDebugQueries
        then DoE $ NoBindS <$> [traceQuery, traceArgs, execQuery]
        else execQuery
        where
          traceQuery = foldr1 AppE [VarE 'traceIO, VarE 'show, VarE sqlQueryName]

          traceArgs = foldr1 AppE . maybe
             [VarE 'traceIO, LitE $ StringL "no arguments"]
             (\name -> [VarE 'traceIO, VarE 'show, VarE name])
             $ argsName

          execQuery = unwrapE opt pgfResult $ foldl1 AppE $ [
                VarE $ maybe 'query_ (const 'query) argsName
              , VarE connName
              , sqlQueryExpr
              ] ++ (maybe [] (return . VarE) argsName)

  let funcDecl = [
          ValD (VarP sqlQueryName) (NormalB sqlQueryExpr) []
        ] ++ (maybe [] (\name -> return $ ValD (VarP name) (NormalB argsExpr) []) argsName)

  return $ FunD funcName [Clause funcArgs funcBody funcDecl]

