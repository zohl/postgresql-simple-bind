{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Database.PostgreSQL.Simple.Bind.Representation
import Data.Text ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let test = \declaration result -> parsePGFunction declaration >>= shouldBe result

  describe "parsePGFunction" $ do
    it "works with simple signatures" $ do
      let r = PGFunction {
          pgfSchema    = ""
        , pgfName      = ""
        , pgfArguments = []
        , pgfResult    = PGSingle ""
        }

      test "function f(x bigint) returns bigint" r {
          pgfName = "f"
        , pgfResult = PGSingle "bigint"
        , pgfArguments = [
              PGArgument { pgaName = "x", pgaType = "bigint", pgaOptional = False }
            ]
        }

      test "function g(x bigint, y varchar) returns void" r {
          pgfName = "g"
        , pgfArguments = [
              PGArgument { pgaName = "x", pgaType = "bigint", pgaOptional = False }
            , PGArgument { pgaName = "y", pgaType = "varchar", pgaOptional = False }
            ]
        , pgfResult = PGSingle "void"
        }

      test "function h() returns varchar" r {
          pgfName = "h"
        , pgfArguments = []
        , pgfResult = PGSingle "varchar"
        }

      test "FUNCTION H(Z VARCHAR) RETURNS BIGINT" r {
          pgfName = "h"
        , pgfArguments = [
              PGArgument { pgaName = "z", pgaType = "varchar", pgaOptional = False }
            ]
        , pgfResult = PGSingle "bigint"
        }


    it "works with default values" $ do
      let r = PGFunction {
          pgfSchema    = ""
        , pgfName      = "f"
        , pgfArguments = []
        , pgfResult    = PGSingle "void"
        }

      test "function f(x bigint default 0::bigint) returns void" r {
          pgfArguments = [
              PGArgument { pgaName = "x", pgaType = "bigint", pgaOptional = True }
            ]
        }

      test "function f(x bigint, y bigint default 0::bigint) returns void" r {
          pgfArguments = [
              PGArgument { pgaName = "x", pgaType = "bigint", pgaOptional = False }
            , PGArgument { pgaName = "y", pgaType = "bigint", pgaOptional = True }
            ]
        }

      test "function f(x bigint, y bigint = 0::bigint, z bigint = 0::bigint) returns void" r {
          pgfArguments = [
              PGArgument { pgaName = "x", pgaType = "bigint", pgaOptional = False }
            , PGArgument { pgaName = "y", pgaType = "bigint", pgaOptional = True }
            , PGArgument { pgaName = "z", pgaType = "bigint", pgaOptional = True }
            ]
        }

    it "works with return value types" $ do
      let r = PGFunction {
          pgfSchema = ""
        , pgfName = "f"
        , pgfArguments = []
        , pgfResult = PGSetOf ""
        }

      test "function f() returns setof bigint" r { pgfResult = PGSetOf "bigint" }

      test "function f() returns SETOF bigint" r { pgfResult = PGSetOf "bigint" }

      test "function f() returns table (x bigint, y varchar)" r {
          pgfResult = PGTable [
              PGColumn { pgcName = "x", pgcType = "bigint" }
            , PGColumn { pgcName = "y", pgcType = "varchar" }
            ]
        }

    it "works with schema names" $ do
      let r = PGFunction {
          pgfSchema = ""
        , pgfName = "f"
        , pgfArguments = []
        , pgfResult = PGSingle "bigint"
        }

      test "function public.f() returns bigint" r { pgfSchema = "public" }

      test "function Test.f() returns bigint" r { pgfSchema = "test" }


  describe "parsing a function with specific type" $ do
    let r = PGFunction {
        pgfSchema = ""
      , pgfName = "f"
      , pgfArguments = []
      , pgfResult = PGSingle "void"
      }

    let x = PGArgument { pgaName = "x", pgaType = "", pgaOptional = False }

    it "works for multiple word types" $ do
      test "function f(x double precision) returns void" r {
          pgfArguments = [ x { pgaType = "double precision" } ]
        }

    it "works for types with precisions" $ do
      test "function f(x bit) returns void" r {
          pgfArguments = [ x { pgaType = "bit" } ]
        }

      test "function f(x bit (32)) returns void" r {
          pgfArguments = [ x { pgaType = "bit" } ]
        }


    it "works for types with precisions and multiple words" $ do
      test "function f(x character varying) returns void" r {
          pgfArguments = [ x { pgaType = "character varying" } ]
        }

      test "function f(x character varying (256)) returns void" r {
          pgfArguments = [ x { pgaType = "character varying" } ]
        }


    it "works for types with multiple precisions" $ do
      test "function f(x numeric) returns void" r {
          pgfArguments = [ x { pgaType = "numeric" } ]
        }

      test "function f(x numeric(10)) returns void" r {
          pgfArguments = [ x { pgaType = "numeric" } ]
        }

      test "function f(x numeric(10,3)) returns void" r {
          pgfArguments = [ x { pgaType = "numeric" } ]
        }

      test "function f(x user_defined_type(1,2,3,4)) returns void" r {
          pgfArguments = [ x { pgaType = "user_defined_type" } ]
        }


    it "works for time types" $ do
      test "function f(x time) returns void" r {
          pgfArguments = [ x { pgaType = "time" } ]
        }

      test "function f(x time (6)) returns void" r {
          pgfArguments = [ x { pgaType = "time" } ]
        }

      test "function f(x time (6) with time zone) returns void" r {
          pgfArguments = [ x { pgaType = "time with time zone" } ]
        }

      test "function f(x time with time zone) returns void" r {
          pgfArguments = [ x { pgaType = "time with time zone" } ]
        }

      test "function f(x time without time zone) returns void" r {
          pgfArguments = [ x { pgaType = "time without time zone" } ]
        }

      test "function f(x timestamp with time zone) returns void" r {
          pgfArguments = [ x { pgaType = "timestamp with time zone" } ]
        }

      test "function f(x timestamptz) returns void" r {
          pgfArguments = [ x { pgaType = "timestamptz" } ]
        }


    it "works for intervals" $ do
      test "function f(x interval) returns void" r {
          pgfArguments = [ x { pgaType = "interval" } ]
        }

      test "function f(x interval month) returns void" r {
          pgfArguments = [ x { pgaType = "interval month" } ]
        }

      test "function f(x interval minute to second (4)) returns void" r {
          pgfArguments = [ x { pgaType = "interval minute to second" } ]
        }

