{-# LANGUAGE CPP #-}

#ifdef DBTests
import Common (withDB, withRollback)
import Data.Text ()
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..))
import ExMessages (specMessages)
import ExNumDumpster (specNumDumpster)
import ExUsers (specUsers)
import Test.Hspec (Spec, hspec)

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
spec conn = mapM_ ($ conn) [
    specNumDumpster
  , specUsers
  , specMessages
  ]
#else
main :: IO ()
main = return ()
#endif
