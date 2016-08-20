# postgresql-simple-bind
[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-(revised))
[![Hackage](https://img.shields.io/hackage/v/postgresql-simple-bind.svg?style=flat)](https://hackage.haskell.org/package/postgresql-simple-bind)
[![Build Status](https://travis-ci.org/zohl/postgresql-simple-bind.svg?branch=master)](https://travis-ci.org/zohl/postgresql-simple-bind)

## Description
  A FFI-like bindings for PostgreSQL strored functions.
  
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

  Another caveat is about functions returning tables (or sets of composite
  types). There is no way to put `not null` constraint on the resulting columns,
  so such function can return result with `null` in any column. At the current
  moment this behaviour is not supported, so each function returning table is
  supposed to return non-`null`-values.


## Customization
  There are not so many ways to change behaviour of `bindFunction` (yet).
  In the most cases the only required tweak is renaming stored functions.
  This can be done by specifying `pboFunctionName` option.
  For example if database and application code adhere snake case and camel case
  naming conventions respectively, conversion can be made like this:

  ```haskell
  import Text.CaseConversion
  import Database.PostgreSQL.Simple.Bind (PostgresBindOptions(..))
  
  bindOptions :: PostgresBindOptions
  bindOptions = PostgresBindOptions {
      pboFunctionName = (\(PGFunction _schema name _args _result) -> convertCase Snake Camel name)
    }
  ```

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
   
  

