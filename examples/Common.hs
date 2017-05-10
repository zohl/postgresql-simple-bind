{-# LANGUAGE LambdaCase #-}

module Common (
    withDB
  , withRollback

  , include
  , initFromDirectory

  , bindOptions
  ) where

import Data.Default (def)
import Control.Exception.Base (bracket)
import Control.Monad (void, when)
import Database.PostgreSQL.Simple (Connection, ConnectInfo, connect, begin, rollback, close, execute_)
import Database.PostgreSQL.Simple.Bind (PostgresBindOptions(..), PGFunction(..), ReturnType(..))
import Database.PostgreSQL.Simple.Types (Query(..))
import Text.CaseConversion (convertCase, WordCase(..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix ((</>))

import qualified Data.ByteString.Char8 as BS

mkFunctionName :: PGFunction -> String
mkFunctionName = convertCase Snake Camel . ("sql_" ++) . pgfName

bindOptions :: PostgresBindOptions
bindOptions = (def :: PostgresBindOptions) {
    pboFunctionName    = mkFunctionName
  , pboSetOfReturnType = \case
      "t_user" -> AsRow
      _        -> AsField
  , pboIsNullable      = isNullable
  , pboDebugQueries    = True
  } where
    isNullable :: String -> String -> Bool
    isNullable "get_new_messages" "contents" = True
    isNullable _                  _          = False


withDB :: ConnectInfo -> (Connection -> IO a) -> IO a
withDB connectInfo = bracket (connect connectInfo) close

withRollback :: (Connection -> IO ()) -> (Connection -> IO ())
withRollback action = \conn -> mapM_ ($ conn) [begin, action, rollback]

include :: Connection -> FilePath -> IO ()
include conn fn = void $ BS.readFile fn >>= (execute_ conn . Query)

includeAll :: Connection -> FilePath -> IO ()
includeAll conn fn = do
  exists <- doesDirectoryExist fn
  when exists $ listDirectory fn >>= mapM_ (include conn) . (fmap (fn </>))

initFromDirectory :: Connection -> FilePath -> IO ()
initFromDirectory conn fn = mapM_ (includeAll conn) . (fmap (fn </>)) $ ["types", "tables", "functions"]
