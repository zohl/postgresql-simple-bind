module Common (
    withDB
  , withRollback
  , include
  , bindOptions
  ) where

import Data.Default (def)
import Control.Exception.Base (bracket)
import Control.Monad (void)
import Database.PostgreSQL.Simple (Connection, ConnectInfo, connect, begin, rollback, close, execute_)
import Database.PostgreSQL.Simple.Bind (PostgresBindOptions(..), PGFunction(..))
import Database.PostgreSQL.Simple.Types (Query(..))
import Text.CaseConversion (convertCase, WordCase(..))
import qualified Data.ByteString.Char8 as BS

mkFunctionName :: PGFunction -> String
mkFunctionName (PGFunction _schema name _args _result)
  = convertCase Snake Camel . ("sql_" ++) $ name

bindOptions :: PostgresBindOptions
bindOptions = (def :: PostgresBindOptions) {
    pboFunctionName = mkFunctionName
  }

withDB :: ConnectInfo -> (Connection -> IO a) -> IO a
withDB connectInfo = bracket (connect connectInfo) close

withRollback :: (Connection -> IO ()) -> (Connection -> IO ())
withRollback action = \conn -> mapM_ ($ conn) [begin, action, rollback]

include :: Connection -> String -> IO ()
include conn fn = void $ BS.readFile fn >>= (execute_ conn . Query)
