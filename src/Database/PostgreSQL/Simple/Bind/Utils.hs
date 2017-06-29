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
  ) where


import Control.Arrow ((***))
import Control.Monad (liftM2)
import Control.Monad.Catch(throwM)
import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query)
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindOptions(..), PostgresBindException(..))
import Database.PostgreSQL.Simple.Bind.Implementation (bindFunction)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..))
import Database.PostgreSQL.Simple.Bind.Parser (parsePGFunction, pgArguments, pgResult)
import Language.Haskell.TH.Syntax (Q, Dec, addDependentFile, runIO)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix ((</>))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- | Bind functions from specified text.
bindFunctionsFromText :: PostgresBindOptions -> Text -> Q [Dec]
bindFunctionsFromText opt s = parsePGFunction s >>= bindFunction opt

-- | Bind functions found in specified file.
bindFunctionsFromFile :: PostgresBindOptions -> FilePath -> Q [Dec]
bindFunctionsFromFile opt fn = do
  addDependentFile fn
  runIO (T.readFile fn)
    >>= bindFunctionsFromText opt

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
bindFunctionsFromDB :: PostgresBindOptions -> Connection -> Maybe String -> String -> Q [Dec]
bindFunctionsFromDB opt conn pgfSchema pgfName = (runIO fetchFunction)
                                             >>= fmap concat . mapM (bindFunction opt) where
  fetchFunction = (query conn sql' (pgfSchema, pgfName))
              >>= mapM (uncurry (liftM2 (,)) . (parse (pgArguments False) *** parse pgResult))
              >>= mapM (\(pgfArguments, pgfResult) -> return PGFunction {..})

  sql' = [sql|
      select pg_catalog.pg_get_function_arguments(p.oid)
           , pg_catalog.pg_get_function_result(p.oid)
      from pg_catalog.pg_proc p
           left join pg_catalog.pg_namespace n on n.oid = p.pronamespace
      where n.nspname = coalesce(?, n.nspname)
        and p.proname ~ ('^('|| ? ||')$')
        and not p.proisagg
        and not p.proiswindow
        and p.prorettype != ('pg_catalog.trigger'::pg_catalog.regtype);
    |]

  parse p s = either
    (\err -> throwM . ParserFailed . concat $ ["In declaration `", T.unpack s, "`: ", err])
    (return)
    (parseOnly p s)
