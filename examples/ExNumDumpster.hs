{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ExistentialQuantification #-}

-- Legend:
--   NumDumpster is a collection of numbers stored in a database.
--   API provides some basic functions to manipulate the collection.
--
-- This is an example of using basic features of the library


module ExNumDumpster (
    specNumDumpster
  ) where

import Common (bindOptions, initFromDirectory)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Bind (bindFunctionsFromDirectory)
import Database.PostgreSQL.Simple.Bind.Types()
import Test.Hspec (Spec, describe, it, shouldBe)

bindFunctionsFromDirectory bindOptions "./examples/sql/numdumpster/functions"


addManyNums :: Connection -> [Int] -> IO ()
addManyNums conn xs = mapM_ (sqlAddNum conn) xs

getSum :: Connection -> IO Int
getSum conn = sum <$> (sqlGetAllNums conn)

iterFib :: Connection -> IO Int
iterFib conn = do
  x  <- sqlGetLastNum conn
  x' <- getSum conn
  sqlClear conn
  addManyNums conn [x, x']
  return x'

specNumDumpster :: Connection -> Spec
specNumDumpster conn = describe "NumDumpster example" $ it "works" $ do
  initFromDirectory conn "./examples/sql/numdumpster"

  sqlAddNum conn 1
  sqlGetLastNum conn >>= shouldBe 1

  sqlClear conn
  addManyNums conn [1, 2, 3, 4]
  sqlGetAllNums conn >>= shouldBe [1, 2, 3, 4]

  sqlGetRange conn Nothing Nothing   >>= shouldBe [1, 2, 3, 4]
  sqlGetRange conn (Just 2) (Just 3) >>= shouldBe [2, 3]
  sqlGetRange conn Nothing (Just 3)  >>= shouldBe [1, 2, 3]
  sqlGetRange conn (Just 2) Nothing  >>= shouldBe [2, 3, 4]

  sqlClear conn
  addManyNums conn [0, 1]
  ((head . reverse) <$> (sequence $ replicate 11 (iterFib conn))) >>= shouldBe 144
