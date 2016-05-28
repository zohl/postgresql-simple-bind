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
    numDumpster
  ) where

import Test.HUnit
import Database.PostgreSQL.Simple

import Database.PostgreSQL.Simple.Bind (bindFunction)      
import Database.PostgreSQL.Simple.Bind.Types()

import Common (bindOptions, TestEnv, mkTest, include)


concat <$> mapM (bindFunction bindOptions) [
    "function add_num(p_x bigint) returns void"
  , "function get_last_num() returns bigint"
  , "function get_range(p_range_min bigint default null, p_range_max bigint default null) returns setof bigint"
  , "function get_all_nums() returns setof bigint"
  , "function clear() returns void"
  ]


addManyNums :: Connection -> [Int] -> IO ()
addManyNums conn xs = sequence_ $ map (sqlAddNum conn) xs

getSum :: Connection -> IO Int
getSum conn = sum <$> (sqlGetAllNums conn)

iterFib :: Connection -> IO Int
iterFib conn = do
  x  <- sqlGetLastNum conn
  x' <- getSum conn
  sqlClear conn
  addManyNums conn [x, x']
  return x'
  

numDumpster :: TestEnv -> Test
numDumpster = mkTest (flip include "./examples/sql/numdumpster.sql")
  (\conn -> do
      sqlAddNum conn 1
      sqlGetLastNum conn >>= \x -> assertEqual "check get_last_num" 1 x
       
      sqlClear conn
      addManyNums conn [1, 2, 3, 4]
      sqlGetAllNums conn >>= \xs -> assertEqual "check get_all_nums" [1, 2, 3, 4] xs

      sqlGetRange conn Nothing Nothing   >>= \xs -> assertEqual "check get_range" [1, 2, 3, 4] xs
      sqlGetRange conn (Just 2) (Just 3) >>= \xs -> assertEqual "check get_range" [2, 3] xs
      sqlGetRange conn Nothing (Just 3)  >>= \xs -> assertEqual "check get_range" [1, 2, 3] xs
      sqlGetRange conn (Just 2) Nothing  >>= \xs -> assertEqual "check get_range" [2, 3, 4] xs

      sqlClear conn
      addManyNums conn [0, 1]
      ((head . reverse) <$> (sequence $ replicate 11 (iterFib conn))) >>=
         \x -> assertEqual "check 11th fibonacci number" 144 x)

