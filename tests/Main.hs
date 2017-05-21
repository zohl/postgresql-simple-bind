{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Catch (throwM)
import Data.Attoparsec.Text (Parser, parseOnly, endOfInput)
import Data.Default (def)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..))
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResult(..))
import Test.Hspec

import qualified Data.Text as T


main :: IO ()
main = hspec spec

testParser :: (Show a, Eq a) => Parser a -> Text -> a -> IO ()
testParser p s r = either
  (\err -> throwM . ParserFailed . concat $ ["In declaration `", T.unpack s, "`: ", err])
  (flip shouldBe r)
  (parseOnly (p <* endOfInput) s) where

spec :: Spec
spec = do

  describe "pgResult" $ do
    let test = testParser pgResult

    it "works with simple types" $ do
      test "bigint" $ PGSingle "bigint"
      test "varchar" $ PGSingle "varchar"

    it "works with SETOF" $ do
      test "setof bigint" $ PGSetOf "bigint"
      test "SETOF bigint" $ PGSetOf "bigint"

    it "works with TABLE" $ do
      test "table (x bigint, y varchar)" $ PGTable [
          PGColumn {pgcName = "x", pgcType = "bigint"}
        , PGColumn {pgcName = "y", pgcType = "varchar"}
        ]


  describe "pgArgument" $ do
    let test = testParser pgArgument

    it "works with simple arguments" $ do
      test "x bigint"  $ PGArgument { pgaMode = def, pgaName = Just "x", pgaType = "bigint",  pgaOptional = False }
      test "y varchar" $ PGArgument { pgaMode = def, pgaName = Just "y", pgaType = "varchar", pgaOptional = False }
      test "Z VARCHAR" $ PGArgument { pgaMode = def, pgaName = Just "z", pgaType = "varchar", pgaOptional = False }

    it "works with argument modes" $ do
      let r = PGArgument { pgaMode = def, pgaName = Just "x", pgaType = "bigint",  pgaOptional = False }
      test "in x bigint"       $ r {pgaMode = In}
      test "out x bigint"      $ r {pgaMode = Out}
      test "inout x bigint"    $ r {pgaMode = InOut}
      test "variadic x bigint" $ r {pgaMode = Variadic}

    it "works with nameless arguments" $ do
      let r = PGArgument { pgaMode = def, pgaName = Nothing, pgaType = "",  pgaOptional = False }
      test "bigint"                            $ r {pgaType = "bigint"}
      test "out varchar"                       $ r {pgaMode = Out, pgaType = "varchar"}
      test "variadic timestamp with time zone" $ r {pgaMode = Variadic, pgaType = "timestamp with time zone"}

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
              PGArgument { pgaMode = def, pgaName = "x", pgaType = "bigint", pgaOptional = False }
            ]
        }

      test "function h() returns varchar" r {
          pgfName = "h"
        , pgfArguments = []
        , pgfResult = PGSingle "varchar"
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
              PGArgument { pgaMode = def, pgaName = "x", pgaType = "bigint", pgaOptional = True }
            ]
        }

      test "function f(x bigint, y bigint default 0::bigint) returns void" r {
          pgfArguments = [
              PGArgument { pgaMode = def, pgaName = "x", pgaType = "bigint", pgaOptional = False }
            , PGArgument { pgaMode = def, pgaName = "y", pgaType = "bigint", pgaOptional = True }
            ]
        }

      test "function f(x bigint, y bigint = 0::bigint, z bigint = 0::bigint) returns void" r {
          pgfArguments = [
              PGArgument { pgaMode = def, pgaName = "x", pgaType = "bigint", pgaOptional = False }
            , PGArgument { pgaMode = def, pgaName = "y", pgaType = "bigint", pgaOptional = True }
            , PGArgument { pgaMode = def, pgaName = "z", pgaType = "bigint", pgaOptional = True }
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

    let x = PGArgument { pgaMode = def, pgaName = "x", pgaType = "", pgaOptional = False }

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

