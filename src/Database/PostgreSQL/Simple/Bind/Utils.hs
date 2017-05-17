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
  Module:      Database.PostgreSQL.Simple.Bind.Utils
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  Misc auxiliary functions.
-}


module Database.PostgreSQL.Simple.Bind.Utils (
    bindFunctionsFromText
  , bindFunctionsFromFile
  , bindFunctionsFromDirectory
  , bindFunctionsFromDB

  , generateBindingsModule
  ) where


import Control.Arrow ((***))
import Data.List (intersperse)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only(..), query)
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindOptions(..), unwrapColumn)
import Database.PostgreSQL.Simple.Bind.Implementation (bindFunction)
import Database.PostgreSQL.Simple.Bind.Parser (parsePGFunction)
import Language.Haskell.TH.Syntax (Q, Dec, addDependentFile, runIO)
import Text.Heredoc (str)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix ((</>))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- | Bind functions from specified text.
bindFunctionsFromText :: PostgresBindOptions -> Text -> Q [Dec]
bindFunctionsFromText opt s = parsePGFunction s >>= bindFunction opt

-- TODO: merge with 'bindFunctionsFromText'
bindFunctionsFromText' :: PostgresBindOptions -> Text -> Q [Dec]
bindFunctionsFromText' opt = bindFunctionsFromText opt
  . fst . T.breakOn " as" . snd . T.breakOnEnd "create "

-- | Bind functions found in specified file.
bindFunctionsFromFile :: PostgresBindOptions -> FilePath -> Q [Dec]
bindFunctionsFromFile opt fn = do
  addDependentFile fn
  runIO (T.readFile fn)
    >>= bindFunctionsFromText' opt

-- | Bind functions found in all files in specified directory.
bindFunctionsFromDirectory :: PostgresBindOptions -> FilePath -> Q [Dec]
bindFunctionsFromDirectory bindOptions fn = do
  exists <- runIO (doesDirectoryExist fn)
  if not exists
     then return []
     else runIO (listDirectory fn)
          >>= fmap concat
          . mapM (bindFunctionsFromFile bindOptions)
          . filter (not . pboIgnoreFiles bindOptions)
          . fmap (fn </>)

-- | Bind functions with specified name from the database.
bindFunctionsFromDB :: PostgresBindOptions -> Connection -> String -> Q [Dec]
bindFunctionsFromDB opt conn name = (runIO getDetails)
  >>= fmap concat
  . mapM (bindFunctionsFromText opt)
  . fmap (uncurry mkDeclaration) where

  getDetails :: IO [(Text, Text)]
  getDetails = query conn sql' (Only . T.pack $ name)

  sql' = [sql|
      select pg_catalog.pg_get_function_arguments(p.oid)
           , pg_catalog.pg_get_function_result(p.oid)
      from pg_catalog.pg_proc p
           left join pg_catalog.pg_namespace n on n.oid = p.pronamespace
      where p.proname ~ ('^('|| ? ||')$')
        and not p.proisagg
        and not p.proiswindow
        and p.prorettype != ('pg_catalog.trigger'::pg_catalog.regtype);
    |]

  mkDeclaration :: Text -> Text -> Text
  mkDeclaration args ret = T.concat $ ["function (", args, ") returns ", ret]


-- | Fetch function declaration(s) from database.
getFunctionDeclaration :: Connection -> String -> IO [String]
getFunctionDeclaration conn name = unwrapColumn <$> query conn sql' (Only $ T.pack name) where
  sql' = [sql|
      select 'function '
          || p.proname
          || '('||pg_catalog.pg_get_function_arguments(p.oid)||')'
          || ' returns '||pg_catalog.pg_get_function_result(p.oid)
      from pg_catalog.pg_proc p
           left join pg_catalog.pg_namespace n on n.oid = p.pronamespace
      where p.proname ~ ('^('|| ? ||')$')
        and not p.proisagg
        and not p.proiswindow
        and p.prorettype != ('pg_catalog.trigger'::pg_catalog.regtype);
    |]

-- | Generate module with bindings.
generateBindingsModule
  :: Connection  -- ^ Database connection
  -> String      -- ^ Full path to 'PostgresBindOptions' record
  -> String      -- ^ Module name
  -> [String]    -- ^ Function names to match
  -> IO String
generateBindingsModule conn opt name ns = do
  ds <- concatMap id <$> mapM (getFunctionDeclaration conn) ns

  let (optPath, optName) = (reverse *** (reverse . drop 1)) . span (/= '.') . reverse $ opt
  let mkList = concat . ("    " :) . intersperse "\n  , "

  return $ concat $ [
      [str| -- This module was automatically generated. Do not edit it.
      |
      |{-# LANGUAGE DataKinds         #-}
      |{-# LANGUAGE OverloadedStrings #-}
      |{-# LANGUAGE TemplateHaskell   #-}
      |{-# LANGUAGE TypeFamilies      #-}
      |
      |]
    , "module ", name, " where "
    , [str|
      |
      |import Database.PostgreSQL.Simple.Bind (bindFunction)
      |]
    , "import ", optPath, " (", optName, ")"
    , [str|
      |import Database.PostgreSQL.Simple.Bind.Types()
      |
      |concat <$> mapM (bindFunction |], optName, [str|) [
      |]
    , (mkList . map (("\"" ++) . (++ "\"")) $ ds)
    , "\n  ]"]


