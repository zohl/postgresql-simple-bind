{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE DeriveGeneric        #-}

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
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.Bind (bindFunction, PostgresType)
import Database.PostgreSQL.Simple.Bind.Types()
import GHC.Generics (Generic)
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
  } deriving (Eq, Show, Generic)

instance FromRow User
type instance PostgresType "t_user" = User


specUsers :: Connection -> Spec
specUsers conn = describe "Users example" $ it "works" $ do
  include conn "./examples/sql/users.sql"

  mrFooId <- sqlAddUser conn "Mr. Foo" 42
  mrBarId <- sqlAddUser conn "Mr. Bar" 53
  mrBazId <- sqlAddUser conn "Mr. Baz" 64

  sqlGetUsers conn (Just "Mr. Foo") >>= \[u] -> (u { userId = 1 }) `shouldBe` (User {
      userId   = 1
    , userName = "Mr. Foo"
    , userAge  = 42
    })

  sqlGetUsers conn Nothing >>= shouldBe [mrFooId, mrBarId, mrBazId] . map userId
  sqlGetUsers conn (Just "Mr. Ba_") >>= shouldBe [mrBarId, mrBazId] . map userId
  sqlDelUser conn mrBarId

  sqlGetUsers conn Nothing >>= shouldBe [mrFooId, mrBazId] . map userId

