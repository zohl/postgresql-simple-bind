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
  Module:      Database.PostgreSQL.Simple.Bind.Util
  Copyright:   (c) 2016 Al Zohali
  License:     GPL3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental
 
  Misc auxiliary functions.
-}


module Database.PostgreSQL.Simple.Bind.Util (
    unwrapRow
  , unwrapColumn
  , Options(..)
  , defaultOptions
  , getFunctionDeclaration    
  , generateBindingsModule
  , mkFunctionName
  ) where


import Data.Text                    (Text, append, pack, breakOnEnd, dropEnd)
import Data.List
import Data.Maybe                   (isNothing)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.Bind.Representation
import Text.Heredoc
import qualified Data.ByteString.Char8 as BS


-- | Options that specify how to construct your function binding.
data Options = Options {
      nameModifier :: String -> String
      -- ^ Function applied to the function's name.

    , schemaModifier :: String -> String
      -- ^ Function applied to the function's schema name.
  }
{-|
  Default 'Options':
  
   @
   'Options' {
     'nameModifier' = id
     'schemaModifier' = ""
   }
   @
-}
defaultOptions = Options {
      nameModifier = id
    , schemaModifier = (\_ -> "")
  }

-- | Given 'Options' and PostgreSQL function representation construct the name of the corresponding haskell function.
mkFunctionName :: Options -> PGFunction -> String
mkFunctionName opt (PGFunction schema name _ _) = concat $ [
    (schemaModifier opt) schema
  , (nameModifier opt) name
  ]


-- | Remove 'Only' constructor.
unwrapColumn :: [Only a] -> [a]
unwrapColumn = map (\(Only x) -> x)

-- | Remove list and 'Only' constructors.
unwrapRow :: [Only a] -> a
unwrapRow = head . unwrapColumn


-- | Fetch function declaration that can be passed to 'bindFunction'.
getFunctionDeclaration :: Connection -> Text -> IO Text
getFunctionDeclaration conn name = unwrapRow <$> query conn sql (Only name) where
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
      |   and p.prorettype != ('pg_catalog.trigger'::pg_catalog.regtype)
      | limit 1;
      |]


-- | Given full path to options, module name and functions' names generate complete module.
generateBindingsModule :: Connection -> Text -> Text -> [Text] -> IO Text
generateBindingsModule conn opt name ns = do
  ds <- mapM (getFunctionDeclaration conn) ns
  let fs = parsePGFunction <$> ds

  let (optPath', optName) = breakOnEnd "." opt
  let optPath = dropEnd 1 optPath'

  let mkList xs = foldl1' append $ ("    " :) $ intersperse "\n  , " xs

  return $ foldl1' append $ [ 
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
    , (mkList $ map (("\"" `append`) . (`append` "\""))  ds)
    , "\n  ]"]
