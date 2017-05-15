{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}

-- Legend:
--   API for reading and sending messages.
--   We apply database patches that modify internal structure yet do not
--   break the API.
--
-- This is a demonstration of the pattern where the database and the
-- application communicate via API. All three times we run the same test
-- against different editions of the database.


module ExMessages (
    specMessages
  ) where

import Common (bindOptions, include, initFromDirectory)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Bind.Types()
import Database.PostgreSQL.Simple.Bind (bindFunctionsFromDirectory)
import Prelude hiding (getContents)
import Test.Hspec (Spec, describe, it, shouldBe)

bindFunctionsFromDirectory bindOptions "./examples/sql/messages/functions"


specMessages :: Connection -> Spec
specMessages conn = describe "Messages example" $ it "works" $ mapM_ runTests [
    initFromDirectory conn "./examples/sql/messages"
  , include conn "./examples/sql/messages/updates/patch-1.sql"
  , include conn "./examples/sql/messages/updates/patch-2.sql"
  ] where

  getId (x, _, _) = x

  runTests update = do
    update

    msg1 <- sqlSendMessage conn "mr_foo" (Just "hello!")
    msg2 <- sqlSendMessage conn "mr_bar" (Just "hello!")
    msg3 <- sqlSendMessage conn "mr_bar" (Just "hello again!")

    sqlGetNewMessages conn "mr_foo" >>= shouldBe [msg1] . map getId
    sqlGetNewMessages conn "mr_bar" >>= shouldBe [msg2, msg3] . map getId

    sqlMarkAsRead conn "mr_bar" msg2

    sqlGetNewMessages conn "mr_bar" >>= shouldBe [msg3] . map getId

    sqlMarkAsRead conn "mr_foo" msg1
    sqlMarkAsRead conn "mr_bar" msg3

    msg4 <- sqlSendMessage conn "mr_baz" Nothing
    sqlGetNewMessages conn "mr_baz" >>= shouldBe [(msg4, Nothing)] . map (\(a, _, b) -> (a, b))
    sqlMarkAsRead conn "mr_baz" msg4
