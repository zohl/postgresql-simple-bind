{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

import Data.Char (ord)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..), tagWith)
import Data.Either (isRight)
import Control.Monad (liftM2)
import Data.Attoparsec.Text (Parser, parseOnly, endOfInput)
import Data.Bifunctor (first)
import Data.Default (def)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Bind.Parser
import Database.PostgreSQL.Simple.Bind.Representation (PGFunction(..), PGArgument(..), PGArgumentMode(..), PGColumn(..), PGResult(..), PGIdentifier(..), PGType(..))
import Text.Heredoc (str)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Arbitrary(..), sized, oneof, choose, suchThat)
import qualified Data.Text as T

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, SomeSymbol(..), someSymbolVal)


class PGSql a where
  render :: a -> Text


data TestPGNormalIdentifier = TestPGNormalIdentifier String deriving (Show)

instance Arbitrary TestPGNormalIdentifier where
  arbitrary = sized $ \n -> fmap TestPGNormalIdentifier $ liftM2 (:)
    (arbitraryChar True)
    (sequence . replicate (n-1) $ arbitraryChar False) where
      arbitraryChar :: Bool -> Gen Char
      arbitraryChar isFirst = oneof $ (if isFirst then id else ([return '$', choose ('0', '9')] ++)) $ [
          return '_'
        , choose ('A', 'Z')
        , choose ('a', 'z')]

instance PGSql TestPGNormalIdentifier where
  render (TestPGNormalIdentifier s) = T.pack s


data TestPGQuotedString (q :: Symbol) = TestPGQuotedString String deriving (Show)

instance (KnownSymbol q) => Arbitrary (TestPGQuotedString q) where
  arbitrary = sized $ \n -> fmap (TestPGQuotedString . concat) . sequence . replicate n $
    (\c -> if c == (head . symbolVal $ (Proxy :: Proxy q)) then [c, c] else [c]) <$>
      (arbitrary `suchThat` ((\x -> x >= 32 && x < 128) . ord))

instance (KnownSymbol q) => PGSql (TestPGQuotedString q) where
  render (TestPGQuotedString s) = T.pack (s ++ (symbolVal (Proxy :: Proxy q)))


propParser :: forall a b. (PGSql a, Arbitrary a, Show a, Show b)
  => Tagged a (Parser b)
  -> String
  -> (b -> Expectation)
  -> Spec
propParser p name t = prop name property where

  property :: a -> Expectation
  property x = test (parseOnly (unTagged p <* endOfInput) (render x))

  test :: Either String b -> Expectation
  test result = either
    (const $ result `shouldSatisfy` isRight)
    t
    result

proxyMap :: Proxy (f :: Symbol -> *) -> Proxy a -> Proxy (f a)
proxyMap _ _ = Proxy

main :: IO ()
main = hspec spec

testParser :: (Show a, Eq a) => Parser a -> Text -> Either ParserException a -> IO ()
testParser parser text result =
  (parseOnly (parser <* endOfInput) text)
  `shouldBe`
  (first ((prefix ++) . show) result) where
    prefix = either id (const "") $ parseOnly (fail "") ""


spec :: Spec
spec = do
  describe "pgNormalIdentifier" $ do
    let prop' = propParser (tagWith (Proxy :: Proxy TestPGNormalIdentifier) pgNormalIdentifier)
    prop' "the first symbol is not '$'" . flip shouldSatisfy $ \s -> (T.head s) /= '$'
    prop' "stored in lowercase" $ \x -> x `shouldBe` (T.toLower x)

  describe "pgQuotedString" $ do
    let prop' q = patternMatch (someSymbolVal [q]) where
          patternMatch (SomeSymbol x) = propParser
            (tagWith (proxyMap (Proxy :: Proxy TestPGQuotedString) x) (pgQuotedString q))

    let qs = ['"', '\'']

    let ps = [
            ("string is surrounded by quotes", \q -> flip shouldSatisfy $ \x -> T.head x == q && T.last x == q)
          , ("internal quotes are doubled", \q -> flip shouldSatisfy $
              null . filter ((/= 0) . (`mod` 2)) . map T.length
                   . filter ((== q) . T.head) . T.group
                   . T.tail . T.init)]

    mapM_
      (\(q, name, test) -> prop' q (name ++ "(" ++ [q] ++ ")") (test q))
      [(q, name, test) | q <- qs, (name, test) <- ps]


  describe "pgComment" $ do
    let test t = testParser pgComment t . Right

    it "works with inline comments" $ do
      test "-- comment without end of line" "-- comment without end of line"
      test "-- comment with end of line\n"  "-- comment with end of line"

    it "works with block comments" $ do
      test "/* simple comment */" "/* simple comment */"
      test
        "/* outer comment /* inner comment */ outer comment */"
        "/* outer comment /* inner comment */ outer comment */"

  describe "pgString" $ do
    let test t = testParser pgString t (Right t)
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
    let test t = testParser pgColumn t . Right
    it "just works" $ do
      test "foo varchar"       PGColumn {pgcName = "foo", pgcType = "varchar"}
      test "foo varchar(16)"   PGColumn {pgcName = "foo", pgcType = "varchar" {pgtModifiers = Just "16"}}
      test "foo varchar(16)[]" PGColumn {pgcName = "foo", pgcType = "varchar[]" {pgtModifiers = Just "16"}}


  describe "pgResult" $ do
    let test t = testParser pgResult t . Right

    it "works with simple types" $ do
      test "bigint" $ PGSingle ["bigint"]
      test "varchar" $ PGSingle ["varchar"]

    it "works with SETOF" $ do
      test "setof bigint" $ PGSetOf ["bigint"]
      test "SETOF bigint" $ PGSetOf ["bigint"]

    it "works with TABLE" $ do
      test "table (x bigint, y varchar)" $ PGTable [
          PGColumn {pgcName = "x", pgcType = "bigint"}
        , PGColumn {pgcName = "y", pgcType = "varchar"}]


  describe "pgArgument" $ do
    let test t = testParser pgArgument t . Right
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


  describe "pgArguments" $ do
    let test t = testParser (pgArguments True) t . Right
    it "works with mixed arguments" $ do
      test "in p1 bigint default 'test', out p2 varchar" [
          PGArgument { pgaMode = In,  pgaName = Just "p1", pgaType = "bigint",  pgaOptional = True }
        , PGArgument { pgaMode = Out, pgaName = Just "p2", pgaType = "varchar", pgaOptional = False }]


  describe "pgArguments (incorrect declarations)" $ do
    let test t = testParser (pgArguments True) t . Left

    it "fails when an optional argument is followed by mandatory one" $ do
      test "p1 bigint, p2 varchar default 'foo', p3 varchar"
        (DefaultValueExpected
          PGArgument { pgaMode = In, pgaName = Just "p3", pgaType = "varchar", pgaOptional = False })

    it "fails when VARIADIC variable followed by non-OUT variable" $ do
      test "variadic p1 bigint, out p2 varchar, in p3 varchar"
        (NonOutVariableAfterVariadic
          PGArgument { pgaMode = In, pgaName = Just "p3", pgaType = "varchar", pgaOptional = False })

    it "fails when OUT or VARIADIC variable specified with default value" $ do
      test "out p bigint default 1"
        (DefaultValueNotExpected
          PGArgument { pgaMode = Out, pgaName = Just "p", pgaType = "bigint", pgaOptional = True })
      test "variadic p bigint default 1"
        (DefaultValueNotExpected
          PGArgument { pgaMode = Variadic, pgaName = Just "p", pgaType = "bigint", pgaOptional = True })


  describe "pgIdentifier" $ do
    let test t = testParser pgIdentifier t . Right
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
    let test t = testParser pgType t . Right

    it "works with simple type names" $ do
      test "varchar"   "varchar"
      test "bigint"    "bigint"
      test "timestamp" "timestamp"

    it "works with multiword type names" $ do
      test "double precision"         "double precision"
      test "character varying"        "character varying"
      test "timestamp with time zone" "timestamp with time zone"

    it "works with types with modifiers" $ do
      test "varchar(256)"               "varchar"           {pgtModifiers = Just "256"}
      test "numeric(10)"                "numeric"           {pgtModifiers = Just "10"}
      test "numeric(10,3)"              "numeric"           {pgtModifiers = Just "10,3"}
      test "character varying(1024)"    "character varying" {pgtModifiers = Just "1024"}
      test "t_type('foo)(bar')"         "t_type"            {pgtModifiers = Just "'foo)(bar'"}
      test "t_type(\"foo)(bar\")"       "t_type"            {pgtModifiers = Just "\"foo)(bar\""}
      test "t_type($$foo)(bar$$)"       "t_type"            {pgtModifiers = Just "$$foo)(bar$$"}
      test "t_type($baz$foo)(bar$baz$)" "t_type"            {pgtModifiers = Just "$baz$foo)(bar$baz$"}

    it "works with time types" $ do
      test "time"                     "time"
      test "time (6)"                 "time"                    {pgtModifiers = Just "6"}
      test "time (6) with time zone"  "time with time zone"     {pgtModifiers = Just "6"}
      test "time with time zone"      "time with time zone"
      test "time without time zone"   "time without time zone"
      test "timestamp with time zone" "timestamp with time zone"
      test "timestamptz"              "timestamptz"

    it "works with intervals" $ do
      test "interval"                       "interval"
      test "interval month"                 "interval month"
      test "interval minute to second (4)"  "interval minute to second" {pgtModifiers = Just "4"}

    it "works with arrays" $ do
      test "varchar []"        "varchar[]"
      test "varchar [10]"      "varchar[]"
      test "varchar [4][4]"    "varchar[][]"
      test "varchar array"     "varchar[]"
      test "varchar array [2]" "varchar[]"
      test "varchar(16)[2]"    "varchar[]" {pgtModifiers = Just "16"}

    it "works with quoted type names" $ do
      test "\"varchar\""        "\"varchar\""
      test "\"varchar\"(16)"    "\"varchar\""   {pgtModifiers = Just "16"}
      test "\"varchar\"(16)[2]" "\"varchar\"[]" {pgtModifiers = Just "16"}

    it "works with column-type expressions" $ do
      test "country.code%type"          "country.code%type"
      test "\"country\".\"code\"%type"  "\"country\".\"code\"%type"

    it "works with user-defined types" $ do
      test "t_custom_type"           "t_custom_type"
      test "t_custom_type (1,2,3,4)" "t_custom_type" {pgtModifiers = Just "1,2,3,4"}

    it "works schema-qualified types" $ do
      test "public.t_custom_type" PGType {
          pgtIdentifier = "t_custom_type" {pgiSchema = Just "public"}
        , pgtModifiers = Nothing}

      test "public.t_custom_type(8)[3][3]" PGType {
          pgtIdentifier = "t_custom_type[][]" {pgiSchema = Just "public"}
        , pgtModifiers = Just "8"}

      test "public.country.code%type" PGType {
          pgtIdentifier = "country.code%type" {pgiSchema = Just "public"}
        , pgtModifiers = Nothing}


  describe "pgFunction" $ do
    let test t = testParser pgFunction t . Right

    it "works with simple declarations" $ do
      test
        [str|create function foo()
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult = PGSingle ["bigint"]
          }

      test
        [str|create or replace function foo()
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult = PGSingle ["bigint"]
          }

      test
        [str|create function foo(p_bar varchar)
            |returns bigint as
            |$$ select 42::bigint $$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = [
                PGArgument {pgaMode = def, pgaName = Just "p_bar", pgaType = "varchar", pgaOptional = False}]
          , pgfResult = PGSingle ["bigint"]
          }

      test
        [str|create function foo(p_bar varchar, p_baz varchar)
            |returns bigint as
            |$body$
            |  select 42::bigint'
            |$body$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = [
                PGArgument {pgaMode = def, pgaName = Just "p_bar", pgaType = "varchar", pgaOptional = False}
              , PGArgument {pgaMode = def, pgaName = Just "p_baz", pgaType = "varchar", pgaOptional = False}]
          , pgfResult = PGSingle ["bigint"]
          }

    it "works with schema-qualified functions" $ do
      test
        [str|create function public.foo()
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Just "public", pgiName = "foo" }
          , pgfArguments = []
          , pgfResult = PGSingle ["bigint"]
          }

    it "works with single OUT parameter" $ do
      test
        [str|create function foo(out p_result bigint) as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["bigint"]
          }

      test
        [str|create function foo(out p_result bigint)
            |returns bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["bigint"]
          }

    it "works with multiple OUT parameters" $ do
      test
        [str|create function foo(out p1 bigint, out p2 varchar) as
            |$$ select 42::bigint, 'test'::varchar $$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["bigint", "varchar"]
          }

      test
        [str|create function foo(out p1 bigint, out p2 varchar)
            |returns record as
            |$$ select 42::bigint, 'test'::varchar $$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["bigint", "varchar"]
          }

    it "works with OUT parameters and SETOF notation" $ do
      test
        [str|create function foo(out p_result bigint)
            |returns setof bigint as
            |'select 42::bigint'|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSetOf ["bigint"]
          }

      test
        [str|create function foo(out p1 bigint, out p2 varchar)
            |returns setof record as
            |$$ select 42::bigint, 'test'::varchar $$|]
        PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSetOf ["bigint", "varchar"]
          }

    it "works with different properties" $ do
      let test' s = test s PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult    = PGSingle ["void"]
          }

      test' [str|create function foo() returns void as '' language 'sql'|]
      test' [str|create function foo() returns void as '' language plpgsql|]

      test' [str|create function foo() returns void as 'test.o', 'foo' language C|]

      test' [str|create function foo() returns void as '' window|]

      test' [str|create function foo() returns void as '' immutable|]
      test' [str|create function foo() returns void as '' stable|]
      test' [str|create function foo() returns void as '' volatile|]

      test' [str|create function foo() returns void as '' leakproof|]
      test' [str|create function foo() returns void as '' not leakproof|]

      test' [str|create function foo() returns void as '' called on null input|]
      test' [str|create function foo() returns void as '' returns null on null input|]
      test' [str|create function foo() returns void as '' strict|]

      test' [str|create function foo() returns void as '' external security invoker|]
      test' [str|create function foo() returns void as '' security definer|]

      test' [str|create function foo() returns void as '' parallel unsafe|]
      test' [str|create function foo() returns void as '' parallel restricted|]
      test' [str|create function foo() returns void as '' parallel safe|]

      test' [str|create function foo() returns void as '' cost 100|]

      test' [str|create function foo() returns void as '' rows 100|]

      test' [str|create function foo() returns void as '' with isStrict|]
      test' [str|create function foo() returns void as '' with isCachable, isStrict|]

  describe "pgFunction (incorrect declarations)" $ do
    let test t = testParser pgFunction t . Left

    it "fails when cannot determine return type" $ do
      test
        "create function foo() as 'select 42::bigint'"
        NoReturnTypeInfo

    it "fails when return types are incoherent" $ do
      test
        [str|create function foo(out p1 bigint, out p2 varchar)
            |returns timestamptz as
            |$$ select 42::bigint, 'test'::varchar $$|]
        (IncoherentReturnTypes
          (PGSingle ["timestamptz"])
          (PGSingle ["bigint", "varchar"]))

      test
        [str|create function foo(out p1 bigint)
            |returns table (p1 bigint) as ''|]
        (IncoherentReturnTypes
          (PGTable [PGColumn {pgcName = "p1", pgcType = "bigint"}])
          (PGSingle ["bigint"]))


  describe "pgDeclarations" $ do
    let test t = testParser pgDeclarations t . Right
    let f = PGFunction {
            pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "foo" }
          , pgfArguments = []
          , pgfResult = PGSingle ["bigint"]
          }

    it "works with single function declaration" $ do
      test
        "create function foo() returns bigint as 'select 42::bigint';"
        [f]

    it "works with multiple function declaration" $ do
      test
        [str|create function foo() returns bigint as 'select 42::bigint';
            |create function bar() returns bigint as 'select 42::bigint';
            |]
        [f, f {pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "bar" }}]

    it "ignores comments" $ do
      test
        [str| -- foo
            |create function foo() returns bigint as 'select 42::bigint';
            |
            | /* bar
            |  *
            |  */
            |create function bar() returns bigint as 'select 42::bigint';
            |]
        [f, f {pgfIdentifier = PGIdentifier { pgiSchema = Nothing, pgiName = "bar" }}]

    it "ignores other clauses" $ do
      test
        [str|create table t (f_id bigint, f_body varchar);
            |insert into t (f_id, f_body) values (1, 'create function foo() bigint as $$ select 1; $$;');
            |insert into t (f_id, f_body) values (2, 'create function bar() bigint as $$ select 2; $$;');
            |commit;
            |
            |perform 'create function baz() returns void as $$ select 3; $$';
            |perform '; create function qux() returns void as $$ select 3; $$; ';
            |
            |]
        []
