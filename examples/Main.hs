{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

import Test.Hspec
import Database.PostgreSQL.Simple.Bind.Representation
import Data.Text ()
import Database.PostgreSQL.Simple
import Common
import ExNumDumpster
-- import ExUsers
-- import ExMessages

connectInfo :: ConnectInfo
connectInfo = ConnectInfo {
      connectHost     = "localhost"
    , connectPort     = 5432
    , connectDatabase = "test_db"
    , connectUser     = "test_role"
    , connectPassword = "TEST"
    }

main :: IO ()
main = withDB connectInfo $ withRollback $ hspec . spec

spec :: Connection -> Spec
spec conn = do
  specNumDumpster conn
