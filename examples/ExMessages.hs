{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Legend:
--   API for reading and sending messages.
--   We apply database patches that modify internal structure yet do not
--   break the API.
--
-- This is a demonstration of the pattern where the database and the
-- application communicate via API. All three times we run the same test
-- against different editions of the database.


module ExMessages (
    messages
  ) where

import Test.HUnit
import Database.PostgreSQL.Simple.Bind (bindFunction)      
import Database.PostgreSQL.Simple.Bind.Types()
import Database.PostgreSQL.Simple (Connection)
import Prelude hiding (getContents)
import Common (bindOptions, TestEnv, mkTest, include)

concat <$> mapM (bindFunction bindOptions) [
    "function send_message(p_receiver varchar, p_contents varchar) returns bigint"
  , "function get_new_messages(p_receiver varchar) returns table (message_id bigint, sender varchar, contents varchar)"
  , "function mark_as_read(p_receiver varchar, p_message_id bigint) returns void"
  ]


runTests :: Int -> Connection -> IO ()
runTests n conn = do
  let getId (x, _, _) = x

  msg1 <- sqlSendMessage conn "mr_foo" "hello!"
  msg2 <- sqlSendMessage conn "mr_bar" "hello!"
  msg3 <- sqlSendMessage conn "mr_bar" "hello again!"

  sqlGetNewMessages conn "mr_foo" >>= \xs ->
    assertEqual ("check get_new_messages " ++ (show n) ++ ".1") [msg1] (map getId xs)

  sqlGetNewMessages conn "mr_bar" >>= \xs ->
    assertEqual ("check get_new_messages " ++ (show n) ++ ".2") [msg2, msg3] (map getId xs)

  sqlMarkAsRead conn "mr_bar" msg2

  sqlGetNewMessages conn "mr_bar" >>= \xs ->
    assertEqual ("check get_new_messages " ++ (show n) ++ ".3") [msg3] (map getId xs)

  sqlMarkAsRead conn "mr_foo" msg1
  sqlMarkAsRead conn "mr_bar" msg3


messages :: TestEnv -> Test
messages = mkTest (flip include "./examples/sql/messages.sql")
  (\conn -> do
     runTests 1 conn
     include conn "./examples/sql/messages-patch-1.sql"
     runTests 2 conn
     include conn "./examples/sql/messages-patch-2.sql"
     runTests 3 conn)

