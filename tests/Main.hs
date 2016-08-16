{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

import Control.Monad (unless)
-- import Data.WithLocation (WithLocation)
import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)
-- import Test.Hspec.Wai (with)
import Database.PostgreSQL.Simple.Bind.Representation
import Data.Text ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "simple signatures" $ do
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

