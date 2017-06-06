{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad.Catch (throwM)
import Data.Attoparsec.Text (Parser, parseOnly, endOfInput)
import Data.Default (def)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Common (PostgresBindException(..))
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResult(..))
import Text.Heredoc (str)
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
  describe "pgString" $ do
    let test s = testParser pgString s s
    it "works with single-quoted strings" $ do
      test "'foo bar baz'"
      test "'foo ''bar'' baz'"
      test "'foo ''''bar'''' baz'"

    it "works with double-quoted strings" $ do
      test "\"foo bar baz\""
      test "\"foo \"\"bar\"\" baz\""
      test "\"foo \"\"\"\"'bar'\"\"\"\" baz\""

    it "works with dollar-quoted strings" $ do
      test "$$foo bar baz$$"
      test "$$foo $ bar baz$$"
      test "$$foo $ bar $ baz$$"

    it "works with tagged dollar-quoted strings" $ do
      test "$qux$foo bar baz$qux$"
      test "$qux$foo $$ bar baz$qux$"
      test "$qux$foo bar $qux baz$qux$"
      test "$qux$foo bar $qux2$ baz$qux$"


  describe "pgColumn" $ do
    let test = testParser pgColumn
    it "just works" $ do
      test "foo varchar"       PGColumn {pgcName = "foo", pgcType = "varchar"}
      test "foo varchar(16)"   PGColumn {pgcName = "foo", pgcType = "varchar"}
      test "foo varchar(16)[]" PGColumn {pgcName = "foo", pgcType = "varchar[]"}


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
        , PGColumn {pgcName = "y", pgcType = "varchar"}]


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

    it "works with positional arguments" $ do
      let r = PGArgument { pgaMode = def, pgaName = Nothing, pgaType = "",  pgaOptional = False }
      test "bigint"                            $ r {pgaType = "bigint"}
      test "out varchar"                       $ r {pgaMode = Out, pgaType = "varchar"}
      test "variadic timestamp with time zone" $ r {pgaMode = Variadic, pgaType = "timestamp with time zone"}

    it "works with different combinations of optional elements" $ do
      let r = PGArgument { pgaMode = def, pgaName = Nothing, pgaType = "timestamp with time zone", pgaOptional = False }
      test "inout foo timestamp with time zone default current_timestamp" r {pgaMode = InOut, pgaName = Just "foo", pgaOptional = True}
      test "inout foo timestamp with time zone"                           r {pgaMode = InOut, pgaName = Just "foo"}
      test "inout     timestamp with time zone default current_timestamp" r {pgaMode = InOut, pgaOptional = True}
      test "inout     timestamp with time zone"                           r {pgaMode = InOut}
      test "      foo timestamp with time zone default current_timestamp" r {pgaName = Just "foo", pgaOptional = True}
      test "      foo timestamp with time zone"                           r {pgaName = Just "foo"}
      test "          timestamp with time zone default current_timestamp" r {pgaOptional = True}
      test "          timestamp with time zone"                           r


  describe "pgIdentifier" $ do
    let test = testParser pgIdentifier
    it "works with simple identifiers" $ do
      test "foo"     $ "foo"
      test "_bar123" $ "_bar123"
      test "baz$$$"  $ "baz$$$"

    it "converts to lower case normal identifiers" $ do
      test "Qux"  $ "qux"
      test "QUUX" $ "quux"

    it "works with quoted identifiers" $ do
      test "\"Corge\""         $ "\"Corge\""
      test "\"Grault\"\"123\"" $ "\"Grault\"\"123\""
      test "\"Waldo !@ #$\""   $ "\"Waldo !@ #$\""


  describe "pgType" $ do
    let test = testParser pgType

    it "works with simple type names" $ do
      test "varchar"   ("varchar"  , Nothing)
      test "bigint"    ("bigint"   , Nothing)
      test "timestamp" ("timestamp", Nothing)

    it "works with multiword type names" $ do
      test "double precision"         ("double precision"        , Nothing)
      test "character varying"        ("character varying"       , Nothing)
      test "timestamp with time zone" ("timestamp with time zone", Nothing)

    it "works with types with modifiers" $ do
      test "varchar(256)"               ("varchar"          , Just "256")
      test "numeric(10)"                ("numeric"          , Just "10")
      test "numeric(10,3)"              ("numeric"          , Just "10,3")
      test "character varying(1024)"    ("character varying", Just "1024")
      test "t_type('foo)(bar')"         ("t_type"           , Just "'foo)(bar'")
      test "t_type(\"foo)(bar\")"       ("t_type"           , Just "\"foo)(bar\"")
      test "t_type($$foo)(bar$$)"       ("t_type"           , Just "$$foo)(bar$$")
      test "t_type($baz$foo)(bar$baz$)" ("t_type"           , Just "$baz$foo)(bar$baz$")

    it "works with time types" $ do
      test "time"                     ("time"                    , Nothing)
      test "time (6)"                 ("time"                    , Just "6")
      test "time (6) with time zone"  ("time with time zone"     , Just "6")
      test "time with time zone"      ("time with time zone"     , Nothing)
      test "time without time zone"   ("time without time zone"  , Nothing)
      test "timestamp with time zone" ("timestamp with time zone", Nothing)
      test "timestamptz"              ("timestamptz"             , Nothing)

    it "works with intervals" $ do
      test "interval"                       ("interval"                 , Nothing)
      test "interval month"                 ("interval month"           , Nothing)
      test "interval minute to second (4)"  ("interval minute to second", Just "4")

    it "works with arrays" $ do
      test "varchar []"        ("varchar[]"  , Nothing)
      test "varchar [10]"      ("varchar[]"  , Nothing)
      test "varchar [4][4]"    ("varchar[][]", Nothing)
      test "varchar array"     ("varchar[]"  , Nothing)
      test "varchar array [2]" ("varchar[]"  , Nothing)
      test "varchar(16)[2]"    ("varchar[]"  , Just "16")

    it "works with quoted type names" $ do
      test "\"varchar\""        ("\"varchar\""  , Nothing)
      test "\"varchar\"(16)"    ("\"varchar\""  , Just "16")
      test "\"varchar\"(16)[2]" ("\"varchar\"[]", Just "16")

    it "works with column-type expressions" $ do
      test "country.code%type"          ("country.code%type"        , Nothing)
      test "\"country\".\"code\"%type"  ("\"country\".\"code\"%type", Nothing)

    it "works with user-defined types" $ do
      test "t_custom_type"           ("t_custom_type", Nothing)
      test "t_custom_type (1,2,3,4)" ("t_custom_type", Just "1,2,3,4")

  describe "pgFunction" $ do
    let test = testParser pgFunction

    it "works with simple declarations" $ do
      test
        [str|create function foo()
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfSchema = ""
          , pgfName   = "foo"
          , pgfArguments = []
          , pgfResult = PGSingle "bigint"
          }

      test
        [str|create or replace function foo()
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfSchema = ""
          , pgfName   = "foo"
          , pgfArguments = []
          , pgfResult = PGSingle "bigint"
          }

      test
        [str|create function foo(p_bar varchar)
            |returns bigint as
            |$$ select 42::bigint $$|]
        PGFunction {
            pgfSchema = ""
          , pgfName   = "foo"
          , pgfArguments = [
                PGArgument {pgaMode = def, pgaName = Just "p_bar", pgaType = "varchar", pgaOptional = False}]
          , pgfResult = PGSingle "bigint"
          }

      test
        [str|create function foo(p_bar varchar, p_baz varchar)
            |returns bigint as
            |$body$
            |  select 42::bigint'
            |$body$|]
        PGFunction {
            pgfSchema = ""
          , pgfName   = "foo"
          , pgfArguments = [
                PGArgument {pgaMode = def, pgaName = Just "p_bar", pgaType = "varchar", pgaOptional = False}
              , PGArgument {pgaMode = def, pgaName = Just "p_baz", pgaType = "varchar", pgaOptional = False}]
          , pgfResult = PGSingle "bigint"
          }
