module Common (
    TestEnv(..)
  , mkTest
  , include
  , bindOptions
  ) where

import Test.HUnit
import Control.Exception.Base
import Text.CaseConversion
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Bind (Options(..), defaultOptions)
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString.Char8 as BS


data TestEnv = TestEnv {
    envConnectInfo :: ConnectInfo
  }

bindOptions :: Options
bindOptions = defaultOptions {
    nameModifier = convertCase Snake Camel . ("sql_" ++)
  }

withConn :: ConnectInfo -> (Connection -> IO a) -> IO a
withConn connectInfo = bracket (connect connectInfo) close

mkTest :: (Connection -> IO ()) -> (Connection -> IO()) -> TestEnv -> Test
mkTest setup run env = TestCase $ withConn (envConnectInfo env)
  (\conn -> mapM_ ($ conn) [begin, setup, run, rollback])

include :: Connection -> String -> IO ()
include conn fn = readFile fn >>= (execute_ conn . Query . BS.pack) >> return ()
