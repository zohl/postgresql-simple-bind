{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Legend:
--   A CRUD-like API for table of users.
--   Due to backward compatibility requirements some of legacy types must be
--   preserved.
--
-- This is an example of using custom types.


module ExUsers (
    specUsers
  ) where

import Common (bindOptions, include)
import Data.Attoparsec.ByteString.Char8 (parseOnly, decimal, char, notChar, many')
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Bind (bindFunction, PostgresType)
import Database.PostgreSQL.Simple.Bind.Types()
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), typename, returnError)
import Test.Hspec (Spec, describe, it, shouldBe)

concat <$> mapM (bindFunction bindOptions) [
    "function get_users(p_filter varchar2 default '') returns setof t_user"
  , "function add_user(p_name varchar2, p_age bigint) returns bigint"
  , "function del_user(p_user_id bigint) returns void"
  ]

type instance PostgresType "varchar2" = Text


data User = User {
    userId   :: Int
  , userName :: String
  , userAge  :: Int
  }

type instance PostgresType "t_user" = User

instance FromField User where
  fromField f v = checkType parseValue where

    checkType cb = (("t_user" /=) <$> typename f) >>= \b -> case b of
      True -> returnError Incompatible f ""
      False -> ($ v) $ maybe (returnError UnexpectedNull f "") cb

    parseValue bs = ($ (parseOnly parser bs)) $ either
      (returnError ConversionFailed f) pure

    parser = do
      _         <- char '('
      userId'   <- decimal
      _         <- char ','
      userName' <- (char '"') *> (many' $ notChar '"') <* (char '"')
      _         <- char ','
      userAge'  <- decimal
      _         <- char ')'
      return $ User { userId = userId', userName = userName', userAge = userAge' }


specUsers :: Connection -> Spec
specUsers conn = describe "Users example" $ it "works" $ do
  include conn "./examples/sql/users.sql"

  mrFooId <- sqlAddUser conn "Mr. Foo" 42
  mrBarId <- sqlAddUser conn "Mr. Bar" 53
  mrBazId <- sqlAddUser conn "Mr. Baz" 64

  sqlGetUsers conn Nothing >>= shouldBe [mrFooId, mrBarId, mrBazId] . map userId
  sqlGetUsers conn (Just "Mr. Ba_") >>= shouldBe [mrBarId, mrBazId] . map userId
  sqlDelUser conn mrBarId

  sqlGetUsers conn Nothing >>= shouldBe [mrFooId, mrBazId] . map userId

