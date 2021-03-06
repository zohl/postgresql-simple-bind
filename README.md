# postgresql-simple-bind
[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-(revised))
[![Hackage](https://img.shields.io/hackage/v/postgresql-simple-bind.svg?style=flat)](https://hackage.haskell.org/package/postgresql-simple-bind)
[![Build Status](https://travis-ci.org/zohl/postgresql-simple-bind.svg?branch=master)](https://travis-ci.org/zohl/postgresql-simple-bind)

## Description
  FFI-like bindings for PostgreSQL strored functions.

  `postgresql-simple-bind` is an extension for `postgresql-simple`
  library that faciliates and automates bindings creation. This is
  especially useful in a design pattern where an application
  communicates with a database via API hiding the internal structure
  of the latter.

## Status
The library is quite stable.
There are no plans to introduce breaking changes into the current API.

## Example
  Suppose we have the following functions in our database:

  ```sql
  function add_num(p_x bigint) returns void
  function get_all_nums() returns setof bigint
  ```

  In order to use them in a haskell application we write the following code:

  ```haskell
  import Data.Default
  import Database.PostgreSQL.Simple.Bind (bindFunction, PostgresBindOptions)
  import Database.PostgreSQL.Simple.Bind.Types()

  bindFunction (def::PostgresBindOptions) "function add_num(p_x bigint) returns void"
  bindFunction (def::PostgresBindOptions) "function get_all_nums() returns setof bigint"
  ```

  That's it. Now we can stick them wherever we want to:
  ```haskell
  add_many_nums :: Connection -> [Int] -> IO ()
  add_many_nums conn xs = sequence_ $ map (add_num conn) xs

  get_sum :: Connection -> IO Int
  get_sum conn = sum <$> (get_all_nums conn)
  ```

## Behind the scenes
  It worth to mention that type translation from PostrgeSQL language to haskell
  is two-step. Firstly, a PostgreSQL type mapped to `PostgresType` instance and
  then this type family provides us the final type.
  For example `add_num` is translated the following way:

  ```haskell
  -- original PostgreSQL declaration
  "function add_num(p_x bigint) returns void"

  -- first step
  add_num :: ( PostgresType "bigint" ~ a, ToField a
             , PostgresType "void" ~ b, FromField b) => Connection -> a -> IO b

  -- second step
  add_num :: Connection -> Int -> IO ()
  ```

  where
  ```haskell
  type instance PostgresType "bigint" = Int
  type instance PostgresType "void"   = ()
  ```
  as they are defined in `Database.PostgreSQL.Simple.Bind.Types`.

  What if the provided instances give us unwanted types (e.g. `varchar` is
  mapped to `Text` while we want `String`)? This is why all the instances are
  defined into a separated module. We just do not import the module and define
  our own instances.


## On types
  As we mentioned in the previous section there are certain restrictions on the
  types that can be used in `PostgresType` instances.

  One of them comes naturally: all argument and result types must be instances of
  `ToField` and `FromField` respectively.

  In case there is an argument with a default value, it's corresponding type
  will be wrapped into `Maybe` constructor.

  Complex types cannot be specified unless there are corresponding `FromRow`
  and/or `ToRow` instances. This means there is no support for `record` return
  type as it doesn't disclose any information on it's structure.



## Customization
### pboFunctionName
  In the most cases the only required tweak is renaming stored functions.
  For example if database and application code adhere snake case and camel case
  naming conventions respectively, conversion can be made like this:

  ```haskell
  import Text.CaseConversion
  import Database.PostgreSQL.Simple.Bind

  bindOptions :: PostgresBindOptions
  bindOptions = PostgresBindOptions {
      pboFunctionName = (\(PGFunction _schema name _args _result) -> convertCase Snake Camel name)
    }
  ```

### pboIsNullable
  PostgreSQL doesn't allow to specify constraints like `not null` on
  columns in functions returning tables. Therefore values returned
  from such functions can be nulls. By default the binding procedure
  assumes that all values aren't nulls, but this can be changed using
  this option. The first argument is a postgres function name and the
  second one is a column name.

### pboSetOfReturnType
  Basically this option allows to choose which instances to
  use: `FromRow` or `FromField` per type. `postgresql-simple` provides
  generics for `FromRow` and `ToRow` instances, so sometimes it's
  easier to employ this mechanism rather than write a parser from
  scratch.

### pboExplicitCasts
  Some sql queries may fail due to ambiguous types. With this option
  enabled every expression will be explicitly casted to function
  argument types.

### pboOlderCallSyntax
  Since version `9.5` PostgreSQL supports `=>` for named notations in
  function calls. Older syntax `:=` is supported for backward
  compatibility.

### pboDebugQueries
  When enabled, every generated function will print to `stdout`
  arguments passed to it. Note, this will fail (compile-time) for
  types without `Show` instances.


## Automated generation
  It can be tedious to manually maintain consistent function declarations
  across the codebase. More convenient way is to automatically generate module
  during the compilation time. In case of cabal it can be done by using
  preBuild hook: set `build-type` to `Custom` in .cabal-file and define
  `main` in Setup.hs like this

  ```haskell
  import Database.PostgreSQL.Simple.Bind.Utils (generateBindingsModule)

  main :: IO ()
  main = defaultMainWithHooks $ simpleUserHooks { preBuild = mkBindings }

  mkBindings :: Args -> BuildFlags -> IO HookedBuildInfo
  mkBindings args buildFlags = do
    conn <- connect connectInfo
    (generateBindingsModule conn
       "Your.Module.customBindOptions" "Bindings" [
            "stored_function_1"
          , "stored_function_2"
          -- ...
          ]) >>= (writeFile "./src/Bindings.hs")
    close conn
    return emptyHookedBuildInfo
  ```

  Every time the build procedure is executed, there will be database
  lookup for function signatures.


## See also
- [Changelog](../master/CHANGELOG.md)
