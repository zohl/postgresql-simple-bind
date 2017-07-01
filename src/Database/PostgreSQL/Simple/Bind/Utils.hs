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
  , bindFunctionsFromDirectoryRecursive
  , bindFunctionsFromDB
  ) where


import Control.Arrow ((***), (&&&), first, second)
import Control.Monad (liftM2)
import Control.Monad.Catch(throwM)
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.List (partition)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query)
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindOptions(..), PostgresBindException(..))
import Database.PostgreSQL.Simple.Bind.Implementation (bindFunction)
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..))
import Database.PostgreSQL.Simple.Bind.Parser (pgDeclarations, pgArguments, pgResult)
import Language.Haskell.TH.Syntax (Q, Dec, addDependentFile, runIO)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix ((</>))
import System.Posix.Files (getFileStatus, isRegularFile, isDirectory)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- | Bind functions from specified text.
bindFunctionsFromText :: PostgresBindOptions -> Text -> Q [Dec]
bindFunctionsFromText opt s = either
  (\err -> throwM . ParserFailed . concat $ ["In declaration `", T.unpack s, "`: ", err])
  (return)
  (parseOnly (pgDeclarations <* endOfInput) s)
  >>= fmap concat . sequence . map (bindFunction opt)

-- | Bind functions found in specified file.
bindFunctionsFromFile :: PostgresBindOptions -> FilePath -> Q [Dec]
bindFunctionsFromFile opt fn = do
  addDependentFile fn
  runIO (T.readFile fn)
    >>= bindFunctionsFromText opt

-- | Bind functions found in all files in specified directory only.
bindFunctionsFromDirectory :: PostgresBindOptions -> FilePath -> Q [Dec]
bindFunctionsFromDirectory = bindFunctionsFromDirectory' False

-- | Bind functions found in all files in specified directory and subdirectories.
bindFunctionsFromDirectoryRecursive :: PostgresBindOptions -> FilePath -> Q [Dec]
bindFunctionsFromDirectoryRecursive = bindFunctionsFromDirectory' True

-- | Bind functions found in all files in specified directory, generic version.
bindFunctionsFromDirectory' :: Bool -> PostgresBindOptions -> FilePath -> Q [Dec]
bindFunctionsFromDirectory' recursive opt fn =
  runIO (getFiles fn) >>= fmap concat . mapM (bindFunctionsFromFile opt) where

    getFiles :: FilePath -> IO [FilePath]
    getFiles d = doesDirectoryExist d >>= \case
      False -> return []
      True  -> do
        (fs, ds) <- listDirectory d
                   >>= mapM ((uncurry fmap) . ((,) &&& getFileStatus)) . (map (d </>))
                   >>= return
                     . (first (filter (not . pboIgnoreFiles opt)))
                     . (map fst *** map fst)                    -- drop file statuses
                     . (fst &&& (fst . snd))                    -- ([regular files], [directories])
                     . (second (partition (isDirectory . snd))) -- ([regular files], ([directories], rest))
                     . (partition (isRegularFile . snd))        -- ([regular files], rest)
        if recursive
          then return fs
          else fmap (fs ++) . fmap concat . mapM getFiles $ ds


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
