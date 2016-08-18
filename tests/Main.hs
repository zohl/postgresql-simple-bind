{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

import Test.Hspec
import Database.PostgreSQL.Simple.Bind.Representation
import Data.Text ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsePGFunction" $ do
    it "works with simple signatures" $ do
      (parsePGFunction "function f(x bigint) returns bigint") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "bigint" False] (PGSingle "bigint"))

      (parsePGFunction "function g(x bigint, y varchar) returns void") `shouldBe`
        (PGFunction "" "g" [PGArgument "x" "bigint" False, PGArgument "y" "varchar" False]
          (PGSingle "void"))

      (parsePGFunction "function h() returns varchar") `shouldBe`
        (PGFunction "" "h" [] (PGSingle "varchar"))

      (parsePGFunction "FUNCTION H(Z VARCHAR) RETURNS BIGINT") `shouldBe`
        (PGFunction "" "h" [PGArgument "z" "varchar" False] (PGSingle "bigint"))


    it "works with default values" $ do
      (parsePGFunction "function f(x bigint default 0::bigint) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "bigint" True] (PGSingle "void"))

      (parsePGFunction "function f(x bigint, y bigint default 0::bigint) returns void") `shouldBe`
        (PGFunction "" "f"
          [PGArgument "x" "bigint" False, PGArgument "y" "bigint" True] (PGSingle "void"))

      (parsePGFunction
        "function f(x bigint, y bigint = 0::bigint, z bigint = 0::bigint) returns void") `shouldBe`
        (PGFunction "" "f" [
            PGArgument "x" "bigint" False
          , PGArgument "y" "bigint" True
          , PGArgument "z" "bigint" True
          ] (PGSingle "void"))


    it "works with return value types" $ do
      (parsePGFunction "function f() returns setof bigint") `shouldBe`
        (PGFunction "" "f" [] (PGSetOf "bigint"))

      (parsePGFunction "function f() returns SETOF bigint") `shouldBe`
        (PGFunction "" "f" [] (PGSetOf "bigint"))

      (parsePGFunction "function f() returns table (x bigint, y varchar)") `shouldBe`
        (PGFunction "" "f" [] (PGTable [PGColumn "x" "bigint", PGColumn "y" "varchar"]))


    it "works with schema names" $ do
      (parsePGFunction "function public.f() returns bigint") `shouldBe`
        (PGFunction "public" "f" [] (PGSingle "bigint"))

      (parsePGFunction "function Test.f() returns bigint") `shouldBe`
        (PGFunction "test" "f" [] (PGSingle "bigint"))



  describe "parsing a function with specific type" $ do
    it "works for multiple word types" $ do
      (parsePGFunction "function f(x double precision) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "double precision" False] (PGSingle "void"))


    it "works for types with precisions" $ do
      (parsePGFunction "function f(x bit) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "bit" False] (PGSingle "void"))

      (parsePGFunction "function f(x bit (32)) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "bit" False] (PGSingle "void"))


    it "works for types with precisions and multiple words" $ do
      (parsePGFunction "function f(x character varying) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "character varying" False] (PGSingle "void"))
      (parsePGFunction "function f(x character varying (256)) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "character varying" False] (PGSingle "void"))


    it "works for types with multiple precisions" $ do
      (parsePGFunction "function f(x numeric) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "numeric" False] (PGSingle "void"))

      (parsePGFunction "function f(x numeric(10)) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "numeric" False] (PGSingle "void"))

      (parsePGFunction "function f(x numeric(10,3)) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "numeric" False] (PGSingle "void"))

      (parsePGFunction "function f(x user_defined_type(1,2,3,4)) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "user_defined_type" False] (PGSingle "void"))


    it "works for time types" $ do
      (parsePGFunction "function f(x time) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "time" False] (PGSingle "void"))

      (parsePGFunction "function f(x time (6)) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "time" False] (PGSingle "void"))

      (parsePGFunction "function f(x time (6) with time zone) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "time with time zone" False] (PGSingle "void"))

      (parsePGFunction "function f(x time with time zone) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "time with time zone" False] (PGSingle "void"))

      (parsePGFunction "function f(x time without time zone) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "time without time zone" False] (PGSingle "void"))

      (parsePGFunction "function f(x timestamp with time zone) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "timestamp with time zone" False] (PGSingle "void"))


    it "works for intervals" $ do
      (parsePGFunction "function f(x interval) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "interval" False] (PGSingle "void"))

      (parsePGFunction "function f(x interval month) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "interval month" False] (PGSingle "void"))

      (parsePGFunction "function f(x interval minute to second (4)) returns void") `shouldBe`
        (PGFunction "" "f" [PGArgument "x" "interval minute to second" False] (PGSingle "void"))

