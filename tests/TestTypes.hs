{-# LANGUAGE OverloadedStrings #-}

module TestTypes (
    typesTests
  ) where

import Test.HUnit
import Database.PostgreSQL.Simple.Bind.Representation

import Data.Text ()

multipleWordsTests :: Test
multipleWordsTests = "multiple words" ~: [
    (parsePGFunction "function f(x double precision) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "double precision" False] (PGSingle "void"))
  ]

preisionsTests :: Test
preisionsTests = "precisions" ~: [
    (parsePGFunction "function f(x bit) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "bit" False] (PGSingle "void"))

  , (parsePGFunction "function f(x bit (32)) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "bit" False] (PGSingle "void"))
  ]

multipleWordsAndPrecisionsTests :: Test
multipleWordsAndPrecisionsTests = "multiple words and precisions" ~: [
    (parsePGFunction "function f(x character varying) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "character varying" False] (PGSingle "void"))

  , (parsePGFunction "function f(x character varying (256)) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "character varying" False] (PGSingle "void"))
  ]

multiplePrecisionsTests :: Test
multiplePrecisionsTests = "multiple precisions" ~: [
    (parsePGFunction "function f(x numeric) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "numeric" False] (PGSingle "void"))

  , (parsePGFunction "function f(x numeric(10)) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "numeric" False] (PGSingle "void"))

  , (parsePGFunction "function f(x numeric(10,3)) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "numeric" False] (PGSingle "void"))
  ]


timeTests :: Test
timeTests = "time" ~: [
    (parsePGFunction "function f(x time) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "time" False] (PGSingle "void"))

  , (parsePGFunction "function f(x time (6)) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "time" False] (PGSingle "void"))

  , (parsePGFunction "function f(x time (6) with time zone) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "time with time zone" False] (PGSingle "void"))

  , (parsePGFunction "function f(x time with time zone) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "time with time zone" False] (PGSingle "void"))

  , (parsePGFunction "function f(x time without time zone) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "time without time zone" False] (PGSingle "void"))

  , (parsePGFunction "function f(x timestamp with time zone) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "timestamp with time zone" False] (PGSingle "void"))
  ]


intervalsTests :: Test
intervalsTests = "intervals" ~: [
    (parsePGFunction "function f(x interval) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "interval" False] (PGSingle "void"))

  , (parsePGFunction "function f(x interval month) returns void")
    ~?= (PGFunction "" "f" [PGArgument "x" "interval month" False] (PGSingle "void"))

  , (parsePGFunction "function f(x interval minute to second (4)) returns void")
      ~?= (PGFunction "" "f" [PGArgument "x" "interval minute to second" False] (PGSingle "void"))
  ]


typesTests :: Test
typesTests = "types" ~: [
    multipleWordsTests
  , preisionsTests
  , multipleWordsAndPrecisionsTests
  , multiplePrecisionsTests
  , intervalsTests
  , timeTests
  ]


