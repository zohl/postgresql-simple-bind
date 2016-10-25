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
{-# LANGUAGE CPP                        #-}

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

#ifdef DebugQueries
import Debug.Trace (traceId, traceShowId)
#endif

import Data.List (intersperse)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGResult(..), PGColumn(..))
import Database.PostgreSQL.Simple.Bind.Representation (parsePGFunction, PostgresBindException(..))
import Database.PostgreSQL.Simple.Bind.Common (unwrapRow, unwrapColumn, PostgresBindOptions(..), ReturnType(..))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..))
import Database.PostgreSQL.Simple.Types (Query(..))
import GHC.TypeLits (Symbol)
import Language.Haskell.TH.Syntax (Q, Dec(..), Exp(..), Type(..), Clause(..), Body(..), Pat(..))
import Language.Haskell.TH.Syntax (Name, mkName, newName, Lit(..), TyLit(..), TyVarBndr(..))
import qualified Data.ByteString.Char8 as BS

-- | Mapping from PostgreSQL types to Haskell types.
type family PostgresType (a :: Symbol)

-- | Function that constructs binding for PostgreSQL stored function by it's signature.
bindFunction :: PostgresBindOptions -> Text -> Q [Dec]
bindFunction opt s = parsePGFunction s >>= mkFunction opt

mkFunction :: PostgresBindOptions -> PGFunction -> Q [Dec]
mkFunction opt f = sequence $ (($ f) . ($ opt)) <$> [mkFunctionT, mkFunctionE]

#ifdef DebugQueries
data Argument = forall a . (Show a, ToField a) => MandatoryArg String a
              | forall a . (Show a, ToField a) => OptionalArg String (Maybe a)

instance Show Argument where
  show (MandatoryArg name value) = "mandatory: " ++ name ++ " => " ++ show value
  show (OptionalArg  name value) = "optional: "  ++ name ++ " => " ++ show value
#else
data Argument = forall a . (ToField a) => MandatoryArg String a
              | forall a . (ToField a) => OptionalArg String (Maybe a)
#endif

instance ToField Argument where
  toField (MandatoryArg _ x)         = toField x
  toField (OptionalArg _ (Just x))   = toField x
  toField (OptionalArg name Nothing) = throw . DefaultValueNotFound $ name

formatArgument :: Argument -> Maybe String
formatArgument (MandatoryArg _name _value)      = Just "?"
formatArgument (OptionalArg name (Just _value)) = Just $ name ++
#ifdef OlderCallSyntax
  " := ?"
#else
  " => ?"
#endif

formatArgument (OptionalArg _name Nothing)      = Nothing

formatArguments :: [Argument] -> String
formatArguments = concat . (intersperse ",") . catMaybes . (map formatArgument)

filterArguments :: [Argument] -> [Argument]
filterArguments = filter isPresented where
  isPresented :: Argument -> Bool
  isPresented (OptionalArg _name Nothing) = False
  isPresented _                           = True


-- | Example: "varchar" -> PostgresType "varchar"
postgresT :: String -> Type
postgresT t = AppT (ConT ''PostgresType) (LitT (StrTyLit t))

-- | Example: ''FromField "varchar" True a -> [PostgresType "varchar" ~ a, FromField (Maybe a)]
mkContextT :: Name -> String -> Bool -> Name -> [Type]
mkContextT constraint typelit nullable name = [
    EqualityT `AppT` (postgresT typelit) `AppT` (VarT name)
  , (ConT constraint) `AppT` result
  ] where
    result = case nullable of
      False -> VarT name
      True  -> (ConT ''Maybe) `AppT` (VarT name)

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
  return ([name], mkContextT ''FromField t False name, VarT name)

mkResultT (PostgresBindOptions {..}) _fname (PGSetOf tname) = do
  name <- newName "y"
  let constraint = case (pboSetOfReturnType tname) of
        AsRow   -> ''FromRow
        AsField -> ''FromField
  return ([name], mkContextT constraint tname False name, ListT `AppT` (VarT name))

mkResultT (PostgresBindOptions {..}) fname (PGTable cs) = do
  names <- sequence $ replicate (length cs) (newName "y")
  let context = concat $ zipWith3
        (\(PGColumn _ typelit) name nullable -> mkContextT ''FromField typelit nullable name)
        cs
        names
        (map (pboIsNullable fname . (\(PGColumn _name ctype) -> ctype)) cs)

  let clause = AppT ListT $ foldl AppT (TupleT (length cs)) $ map VarT names
  return (names, context, clause)

-- | Example: [PGArgument "x" "varchar" True, PGArgument "y" "bigint" False] -> (
--       ["x1", "x2"]
--     , [PostgresType "varchar" ~ x1, ToField x1, PostgresType "bigint" ~ x2, ToField x2]
--     , [x1, Maybe x2])
mkArgsT :: [PGArgument] -> Q ([Name], [Type], [Type])
mkArgsT cs = do
  names <- sequence $ replicate (length cs) (newName "x")
  let context = concat $ zipWith (\(PGArgument _ t _) n -> mkContextT ''ToField t False n) cs names

  let defWrap d = case d of
        True  -> AppT (ConT ''Maybe)
        False -> id

  let clause = zipWith (\(PGArgument _ _ d) -> (defWrap d) . VarT) cs names
  return (names, context, clause)

-- | Example: (PGFunction "public" "foo"
--     [PGArgument "x" "varchar" True, PGArgument "y" "bigint" False] (PGSingle "varchar")) -> {
--         foo :: forall x1 x2 x3. (
--             PostgresType "varchar" ~ x1, ToField x1
--           , PostgresType "bigint" ~ x2, ToField x2
--           , PostgresType "varchar" ~ x2, FromField x3) => Connection -> x1 -> Maybe x2 -> x3}
mkFunctionT :: PostgresBindOptions -> PGFunction -> Q Dec
mkFunctionT opt@(PostgresBindOptions {..}) f@(PGFunction _schema fname args ret) = do
  (argNames, argContext, argClause) <- mkArgsT args
  (retNames, retContext, retClause) <- mkResultT opt fname ret

  let vars = map PlainTV (argNames ++ retNames)
  let context = argContext ++ retContext

  let chain x = AppT (AppT ArrowT x)
  let clause = foldr1 chain $ (ConT ''Connection):(argClause ++ [AppT (ConT ''IO) retClause])

  return $ SigD (mkName $ pboFunctionName f) $ ForallT vars context clause


traceIdWrapE :: Exp -> Exp
#ifdef DebugQueries
traceIdWrapE q = (VarE 'traceId) `AppE` q
#else
traceIdWrapE = id
#endif

-- | Example:
--     (PGFunction "public" "foo"
--       [PGArgument "x" "varchar" True, PGArgument "y" "bigint" False] (PGSingle "varchar"))
--     args -> { Query $ BS.pack $ concat ["select public.foo (", (formatArguments args), ")"] }
mkSqlQuery :: PostgresBindOptions -> PGFunction -> Maybe Name -> Exp
mkSqlQuery opt (PGFunction schema fname _args ret) argsName =
  toQuery . traceIdWrapE . AppE (VarE 'concat) . ListE $ [
      mkStrLit $ concat [prefix opt, " ", functionName, "("]
    , maybe (mkStrLit "") (\args -> (VarE 'formatArguments) `AppE` (VarE args)) argsName
    , mkStrLit ")"] where

  prefix (PostgresBindOptions {..}) = case ret of
    PGTable _     -> mkSelect AsRow
    PGSetOf tname -> mkSelect $ pboSetOfReturnType tname
    _             -> mkSelect AsField

  mkSelect AsRow   = "select * from"
  mkSelect AsField = "select"

  functionName = case schema of
    "" -> fname
    _  -> schema ++ "." ++ fname

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

traceShowIdWrapE :: Exp -> Exp
#ifdef DebugQueries
traceShowIdWrapE q = (VarE 'traceShowId) `AppE` q
#else
traceShowIdWrapE = id
#endif

-- | Example: (PGFunction "public" "foo"
--     [PGArgument "x" "varchar" True, PGArgument "y" "bigint" False] (PGSingle "varchar")) -> {
--       foo conn x1 x2 = query conn
--         (Query $ BS.pack $ concat ["select public.foo (", (formatArguments args), ")"])
--         (filterArguments [MandatoryArg "x1" x1, OptionalArg "x2" x2])
mkFunctionE :: PostgresBindOptions -> PGFunction -> Q Dec
mkFunctionE opt@(PostgresBindOptions {..}) f@(PGFunction _schema _fname args ret) = do
  names <- sequence $ replicate (length args) (newName "x")
  connName <- newName "conn"

  argsName <- case (null args) of
    True  -> return Nothing
    False -> Just <$> newName "args"

  let wrapArg (PGArgument n _ d) argName = foldl1 AppE $ case d of
        False -> [(ConE 'MandatoryArg), (LitE (StringL n)), (VarE argName)]
        True  -> [(ConE 'OptionalArg), (LitE (StringL n)), (VarE argName)]

  let argsExpr = (VarE 'filterArguments) `AppE` (ListE $ zipWith wrapArg args names)

  let funcName = mkName $ pboFunctionName f
  let funcArgs = (VarP connName):(map VarP names)
  let funcBody = NormalB $ unwrapE opt ret $ foldl1 AppE $ [
          VarE $ maybe 'query_ (const 'query) argsName
        , VarE connName
        , mkSqlQuery opt f argsName
        ] ++ (const (traceShowIdWrapE argsExpr) <$> maybeToList argsName)

  let decl = (\name -> ValD (VarP name) (NormalB argsExpr) []) <$> maybeToList argsName
  return $ FunD funcName [Clause funcArgs funcBody decl]

