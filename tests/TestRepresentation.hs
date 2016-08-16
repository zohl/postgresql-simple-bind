{-# LANGUAGE OverloadedStrings #-}

module TestRepresentation (
    testRepresentation
  ) where

import Test.HUnit
import Database.PostgreSQL.Simple.Bind.Representation

import Data.Text ()
import TestTypes


defaultValuesTests :: Test
defaultValuesTests = "default values" ~: [
    (parsePGFunction "function f(x bigint default 0::bigint) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "bigint" True] (PGSingle "void"))

  , (parsePGFunction "function f(x bigint, y bigint default 0::bigint) returns void")
    ~?= (PGFunction "" "f"
          [PGArgument "x" "bigint" False, PGArgument "y" "bigint" True] (PGSingle "void"))

  , (parsePGFunction
       "function f(x bigint, y bigint = 0::bigint, z bigint = 0::bigint) returns void")
    ~?= (PGFunction "" "f" [
            PGArgument "x" "bigint" False
          , PGArgument "y" "bigint" True
          , PGArgument "z" "bigint" True
          ] (PGSingle "void"))
  ]

returnValuesTests :: Test
returnValuesTests = "return values" ~: [
    (parsePGFunction "function f() returns setof bigint")
    ~?= (PGFunction "" "f" [] (PGSetOf "bigint"))

  , (parsePGFunction "function f() returns SETOF bigint")
    ~?= (PGFunction "" "f" [] (PGSetOf "bigint"))

  , (parsePGFunction "function f() returns table (x bigint, y varchar)")
    ~?= (PGFunction "" "f" [] (PGTable [PGColumn "x" "bigint", PGColumn "y" "varchar"]))
  ]

schemaTests :: Test
schemaTests = "schema" ~: [
    (parsePGFunction "function public.f() returns bigint")
    ~?= (PGFunction "public" "f" [] (PGSingle "bigint"))

  , (parsePGFunction "function Test.f() returns bigint")
    ~?= (PGFunction "test" "f" [] (PGSingle "bigint"))
  ]


testRepresentation :: Test
testRepresentation = TestList $ [
    simpleTests
  , defaultValuesTests
  , returnValuesTests
  , returnValuesTests
  , schemaTests
  , typesTests
  ]

