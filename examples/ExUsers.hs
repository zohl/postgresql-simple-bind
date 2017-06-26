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

import Common (bindOptions, initFromDirectory)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.Bind (PostgresType)
import Database.PostgreSQL.Simple.Bind.Types()
import Database.PostgreSQL.Simple.Bind (bindFunctionsFromDirectory)
import GHC.Generics (Generic)
import Test.Hspec (Spec, describe, it, shouldBe)

bindFunctionsFromDirectory bindOptions "./examples/sql/users/functions"


type instance PostgresType "varchar2" = Text


data User = User {
    userId   :: Int
  , userName :: Text
  , userAge  :: Int
  } deriving (Eq, Show, Generic)

instance FromRow User
type instance PostgresType "t_user" = User


specUsers :: Connection -> Spec
specUsers conn = describe "Users example" $ it "works" $ do
  initFromDirectory conn "./examples/sql/users"

  mrFooId <- sqlAddUser conn "Mr. Foo" 42
  mrBarId <- sqlAddUser conn "Mr. Bar" 53
  mrBazId <- sqlAddUser conn "Mr. Baz" 64

  sqlGetUsers conn (Just "Mr. Foo") >>= \[u] -> (u { userId = 1 }) `shouldBe` (User {
      userId   = 1
    , userName = "Mr. Foo"
    , userAge  = 42
    })

  users <- sqlGetUsers conn Nothing
  (map userId users) `shouldBe` [mrFooId, mrBarId, mrBazId]
  sqlGetUsersEx conn Nothing >>= shouldBe (map (\User {..} -> (userId, userName, userAge)) users)

  sqlGetUsers conn (Just "Mr. Ba_") >>= shouldBe [mrBarId, mrBazId] . map userId
  sqlDelUser conn mrBarId

  sqlGetUsers conn Nothing >>= shouldBe [mrFooId, mrBazId] . map userId

