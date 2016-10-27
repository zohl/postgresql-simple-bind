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

import Common (bindOptions, include)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Bind (bindFunction)
import Database.PostgreSQL.Simple.Bind.Types()
import Prelude hiding (getContents)
import Test.Hspec (Spec, describe, it, shouldBe)

concat <$> mapM (bindFunction bindOptions) [
    "function send_message(p_receiver varchar, p_contents varchar default null) returns bigint"
  , "function get_new_messages(p_receiver varchar) returns table (message_id bigint, sender varchar, contents varchar)"
  , "function mark_as_read(p_receiver varchar, p_message_id bigint) returns void"
  ]


specMessages :: Connection -> Spec
specMessages conn = describe "Messages example" $ it "works" $ mapM_ runTests [
    "./examples/sql/messages.sql"
  , "./examples/sql/messages-patch-1.sql"
  , "./examples/sql/messages-patch-2.sql"
  ] where

  getId (x, _, _) = x

  runTests fn = do
    include conn fn

    msg1 <- sqlSendMessage conn "mr_foo" (Just "hello!")
    msg2 <- sqlSendMessage conn "mr_bar" (Just "hello!")
    msg3 <- sqlSendMessage conn "mr_bar" (Just "hello again!")

    sqlGetNewMessages conn "mr_foo" >>= shouldBe [msg1] . map getId
    sqlGetNewMessages conn "mr_bar" >>= shouldBe [msg2, msg3] . map getId

    sqlMarkAsRead conn "mr_bar" msg2

    sqlGetNewMessages conn "mr_bar" >>= shouldBe [msg3] . map getId

    sqlMarkAsRead conn "mr_foo" msg1
    sqlMarkAsRead conn "mr_bar" msg3

    sqlSendMessage conn "mr_foo" Nothing
