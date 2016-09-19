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
import Data.List (intersperse)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGResult(..), PGColumn(..))
import Database.PostgreSQL.Simple.Bind.Representation (parsePGFunction, PostgresBindException(..))
import Database.PostgreSQL.Simple.Bind.Common (unwrapRow, unwrapColumn, PostgresBindOptions(..))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField(..))
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

data Argument = forall a . (ToField a) => MandatoryArg String a
              | forall a . (ToField a) => OptionalArg String (Maybe a)

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

-- | Example: "varchar" FromField a -> [PostgresType "varchar" ~ a, FromField a]
mkContextT :: Name -> String -> Name -> [Type]
mkContextT c t n = [
    EqualityT `AppT` (postgresT t) `AppT` (VarT n)
  , (ConT c) `AppT` (VarT n)] where

-- | Examples:
--     (PGSingle "varchar") -> (["y"], [PostgresType "varchar" ~ y, FromField y], y)
--     (PGSetOf "varchar") -> (["y"], [PostgresType "varchar" ~ y, FromField y], [y])
--     (PGTable ["bigint", "varchar"])  -> (
--         ["y", "z"]
--       , [PostgresType "bigint" ~ y, FromField y, PostgresType "varchar" ~ z, FromField z]
--       , (y, z))
mkResultT :: PGResult -> Q ([Name], [Type], Type)
mkResultT (PGSingle t) = do
  name <- newName "y"
  return ([name], mkContextT ''FromField t name, VarT name)

mkResultT (PGSetOf t) = do
  (names, context, clause) <- mkResultT (PGSingle t)
  return (names, context, AppT ListT clause)

mkResultT (PGTable cs) = do
  names <- sequence $ replicate (length cs) (newName "y")
  let context = concat $ zipWith (\(PGColumn _ t) n -> mkContextT ''FromField t n) cs names
  let clause = AppT ListT $ foldl AppT (TupleT (length cs)) $ map VarT names
  return (names, context, clause)

-- | Example: [PGArgument "x" "varchar" True, PGArgument "y" "bigint" False] -> (
--       ["x1", "x2"]
--     , [PostgresType "varchar" ~ x1, ToField x1, PostgresType "bigint" ~ x2, ToField x2]
--     , [x1, Maybe x2])
mkArgsT :: [PGArgument] -> Q ([Name], [Type], [Type])
mkArgsT cs = do
  names <- sequence $ replicate (length cs) (newName "x")
  let context = concat $ zipWith (\(PGArgument _ t _) n -> mkContextT ''ToField t n) cs names

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
mkFunctionT (PostgresBindOptions {..}) f@(PGFunction _schema _name args ret) = do
  (argNames, argContext, argClause) <- mkArgsT args
  (retNames, retContext, retClause) <- mkResultT ret

  let vars = map PlainTV (argNames ++ retNames)
  let context = argContext ++ retContext

  let chain x = AppT (AppT ArrowT x)
  let clause = foldr1 chain $ (ConT ''Connection):(argClause ++ [AppT (ConT ''IO) retClause])

  return $ SigD (mkName $ pboFunctionName f) $ ForallT vars context clause


-- | Example:
--     (PGFunction "public" "foo"
--       [PGArgument "x" "varchar" True, PGArgument "y" "bigint" False] (PGSingle "varchar"))
--     args -> { Query $ BS.pack $ concat ["select public.foo (", (formatArguments args), ")"] }
mkSqlQuery :: PGFunction -> Maybe Name -> Exp
mkSqlQuery (PGFunction schema name _args ret) argsName = toQuery $ AppE (VarE 'concat) $ ListE [
      mkStrLit $ concat [prefix, " ", functionName, "("]
    , maybe (mkStrLit "") (\args -> (VarE 'formatArguments) `AppE` (VarE args)) argsName
    , mkStrLit ")"] where

  prefix = case ret of
    PGTable _ -> "select * from"
    _         -> "select"

  functionName = case schema of
    "" -> name
    _  -> schema ++ "." ++ name

  mkStrLit s = LitE (StringL s)
  toQuery = AppE (ConE 'Query) . AppE (VarE 'BS.pack)

-- | Examples:
--     (PGSingle _) q -> { unwrapRow  q }
--     (PGSetOf  _) q -> { unwrapColumn q }
--     (PGTable  _) q -> { q }
unwrapE :: PGResult -> Exp -> Exp
unwrapE (PGSingle _) q = (VarE 'fmap) `AppE` (VarE 'unwrapRow) `AppE` q
unwrapE (PGSetOf _) q = (VarE 'fmap) `AppE` (VarE 'unwrapColumn) `AppE` q
unwrapE (PGTable _) q = q

-- | Example: (PGFunction "public" "foo"
--     [PGArgument "x" "varchar" True, PGArgument "y" "bigint" False] (PGSingle "varchar")) -> {
--       foo conn x1 x2 = query conn
--         (Query $ BS.pack $ concat ["select public.foo (", (formatArguments args), ")"])
--         (filterArguments [MandatoryArg "x1" x1, OptionalArg "x2" x2])
mkFunctionE :: PostgresBindOptions -> PGFunction -> Q Dec
mkFunctionE (PostgresBindOptions {..}) f@(PGFunction _schema _name args ret) = do
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
  let funcBody = NormalB $ unwrapE ret $ foldl1 AppE $ [
          VarE $ maybe 'query_ (const 'query) argsName
        , VarE connName
        , mkSqlQuery f argsName
        ] ++ (const argsExpr <$> maybeToList argsName)

  let decl = (\name -> ValD (VarP name) (NormalB argsExpr) []) <$> maybeToList argsName
  return $ FunD funcName [Clause funcArgs funcBody decl]
