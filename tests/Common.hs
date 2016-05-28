module Common (
    TestEnv(..)
  , mkTest
  , include
  , bindOptions
  ) where

import Test.HUnit
import Control.Exception.Base
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Bind (Options(..), defaultOptions)
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString.Char8 as BS



