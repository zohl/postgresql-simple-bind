{-# LANGUAGE NamedFieldPuns #-}

import Test.HUnit
import Control.Monad
import System.IO
import System.Exit
import Database.PostgreSQL.Simple

import Common          (TestEnv(..), connectInfo)
import ExNumDumpster
import ExUsers
import ExMessages

testEnv :: TestEnv
testEnv = TestEnv {
    connectInfo = ConnectInfo {
        connectHost     = "localhost"
      , connectPort     = 5432
      , connectDatabase = "test"
      , connectUser     = "test"
      , connectPassword = "TEST"
      }
  }

tests :: [TestEnv -> Test]
tests = [
      TestLabel "NumDumpster" . numDumpster
    , TestLabel "Users"       . users
    , TestLabel "Messages"    . messages
    ]

main :: IO ()
main = do
  mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
 
  Counts {cases, tried, errors, failures} <- runTestTT $ TestList $ map ($ testEnv) tests
  when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure

