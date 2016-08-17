module Common (
    withDB
  , withRollback
  , include
  , bindOptions
  ) where

import Control.Exception.Base (bracket)
import Database.PostgreSQL.Simple (Connection, ConnectInfo, connect, begin, rollback, close, execute_)
import Database.PostgreSQL.Simple.Bind (Options(..), defaultOptions)
import Database.PostgreSQL.Simple.Types (Query(..))
import Text.CaseConversion (convertCase, WordCase(..))
import qualified Data.ByteString.Char8 as BS

bindOptions :: Options
bindOptions = defaultOptions {
    nameModifier = convertCase Snake Camel . ("sql_" ++)
  }

withDB :: ConnectInfo -> (Connection -> IO a) -> IO a
withDB connectInfo = bracket (connect connectInfo) close

withRollback :: (Connection -> IO ()) -> (Connection -> IO ())
withRollback action = \conn -> mapM_ ($ conn) [begin, action, rollback]

include :: Connection -> String -> IO ()
include conn fn = readFile fn >>= (execute_ conn . Query . BS.pack) >> return ()
