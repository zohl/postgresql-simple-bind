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
    getFunctionDeclaration
  , generateBindingsModule
  ) where


import Control.Arrow ((***))
import Data.List (intersperse)
import Database.PostgreSQL.Simple (Connection, Only(..), query)
import Database.PostgreSQL.Simple.Bind.Common (unwrapColumn)
import Database.PostgreSQL.Simple.Types (Query(..))
import Text.Heredoc (str)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

-- | Fetch function declaration(s) from database.
getFunctionDeclaration :: Connection -> String -> IO [String]
getFunctionDeclaration conn name = unwrapColumn <$> query conn sql (Only $ T.pack name) where
  sql = Query $ BS.pack $ [str|
      | select 'function '
      |     || p.proname
      |     || '('||pg_catalog.pg_get_function_arguments(p.oid)||')'
      |     || ' returns '||pg_catalog.pg_get_function_result(p.oid)
      | from pg_catalog.pg_proc p
      |      left join pg_catalog.pg_namespace n on n.oid = p.pronamespace
      | where p.proname ~ ('^('|| ? ||')$')
      |   and not p.proisagg
      |   and not p.proiswindow
      |   and p.prorettype != ('pg_catalog.trigger'::pg_catalog.regtype);
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

