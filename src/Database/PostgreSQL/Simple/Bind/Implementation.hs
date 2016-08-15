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

{-|
  Module:      Database.PostgreSQL.Simple.Bind.Implementation
  Copyright:   (c) 2016 Al Zohali
  License:     GPL3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental
-}


module Database.PostgreSQL.Simple.Bind.Implementation (
    bindFunction
  , PostgresType
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Bind.Representation
import Database.PostgreSQL.Simple.Bind.Util (unwrapRow, unwrapColumn, mkFunctionName, Options(..))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import GHC.TypeLits
import Language.Haskell.TH.Syntax
import qualified Data.ByteString.Char8 as BS


-- | Mapping from PostgreSQL types to Haskell types.
type family PostgresType (a :: Symbol)


-- | Function that constructs binding for PostgreSQL stored function by it's signature.
bindFunction :: Options -> Text -> Q [Dec]
bindFunction opt = (mkFunction opt) . parsePGFunction



data Argument = forall a . (ToField a) => MandatoryArg String a
              | forall a . (ToField a) => OptionalArg String (Maybe a)

instance ToField Argument where
  toField (MandatoryArg _ x) = toField x
  toField (OptionalArg _ (Just x)) = toField x
  toField (OptionalArg _ Nothing) = error "OptionalArg::toField: encountered Nothing"


formatArgument :: Argument -> String
formatArgument (MandatoryArg _name _) = "?"
formatArgument (OptionalArg name (Just _)) = name ++ " := ?"
formatArgument (OptionalArg _ Nothing) = error "TODO"


formatArguments :: [Argument] -> String
formatArguments = concat . (intersperse ",") . (map formatArgument)


isActual :: Argument -> Bool
isActual (OptionalArg _ Nothing)  = False
isActual _                        = True

filterArguments :: [Argument] -> [Argument]
filterArguments = filter isActual




mkFunction :: Options -> PGFunction -> Q [Dec]
mkFunction opt f = sequence $ (($ f) . ($ opt)) <$> [mkFunctionT, mkFunctionE]

postgresT :: String -> Type
postgresT t = AppT (ConT ''PostgresType) (LitT (StrTyLit t))

mkContextT :: Name -> String -> Name -> [Type]
mkContextT c t n = [
    EqualityT `AppT` (postgresT t) `AppT` (VarT n)
  , (ConT c) `AppT` (VarT n)] where


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


mkArgsT :: [PGArgument] -> Q ([Name], [Type], [Type])
mkArgsT cs = do
  names <- sequence $ replicate (length cs) (newName "x")
  let context = concat $ zipWith (\(PGArgument _ t _) n -> mkContextT ''ToField t n) cs names

  let defWrap d = case d of
        True  -> AppT (ConT ''Maybe)
        False -> id

  let clause = zipWith (\(PGArgument _ _ d) -> (defWrap d) . VarT) cs names
  return (names, context, clause)


mkFunctionT :: Options -> PGFunction -> Q Dec
mkFunctionT opt f@(PGFunction _schema _name args ret) = do
  (argNames, argContext, argClause) <- mkArgsT args
  (retNames, retContext, retClause) <- mkResultT ret

  let vars = map PlainTV (argNames ++ retNames)
  let context = argContext ++ retContext

  let chain x = AppT (AppT ArrowT x)
  let clause = foldr1 chain $ (ConT ''Connection):(argClause ++ [AppT (ConT ''IO) retClause])

  return $ SigD (mkName (mkFunctionName opt f)) $ ForallT vars context clause



mkSqlQuery :: PGFunction -> Name -> Exp
mkSqlQuery (PGFunction schema name _args ret) argsName = toQuery $ foldr1 AppE [
      (AppE (VarE '(++)) (mkStrLit $ concat [prefix, " ", functionName, "("]))
    , (AppE (VarE '(++)) ((VarE 'formatArguments) `AppE` (VarE argsName)))
    , (mkStrLit ")")] where

  prefix = case ret of
    PGTable _ -> "select * from"
    _         -> "select"

  functionName = case schema of
    "" -> name
    _  -> schema ++ "." ++ name

  mkStrLit s = LitE (StringL s)

  toQuery = AppE (ConE 'Query) . AppE (VarE 'BS.pack)



unwrapE :: PGResult -> Exp -> Exp
unwrapE (PGSingle _) q = (VarE 'fmap) `AppE` (VarE 'unwrapRow) `AppE` q
unwrapE (PGSetOf _) q = (VarE 'fmap) `AppE` (VarE 'unwrapColumn) `AppE` q
unwrapE (PGTable _) q = q


mkFunctionE :: Options -> PGFunction -> Q Dec
mkFunctionE opt f@(PGFunction _schema _name args ret) = do
  names <- sequence $ replicate (length args) (newName "x")

  connName <- newName "conn"
  argsName <- newName "args"

  let funcName = mkName $ mkFunctionName opt f

  let funcArgs = (VarP connName):(map VarP names)

  let funcBody = NormalB $ unwrapE ret $ foldl1 AppE [
          (VarE 'query)
        , (VarE connName)
        , (mkSqlQuery f argsName)
        , (VarE argsName)]

  let wrapArg (PGArgument n _ d) argName = foldl1 AppE $ case d of
        False -> [(ConE 'MandatoryArg), (LitE (StringL n)), (VarE argName)]
        True  -> [(ConE 'OptionalArg), (LitE (StringL n)), (VarE argName)]

  let argsBody = NormalB $ (VarE 'filterArguments) `AppE` (ListE $ zipWith wrapArg args names)

  return $ FunD funcName [Clause funcArgs funcBody [ValD (VarP argsName) argsBody []]]

