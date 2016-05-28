{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

import Test.HUnit
import Control.Monad
import System.IO
import System.Exit

import TestRepresentation


tests :: [Test]
tests = [
      TestLabel "Representation" testRepresentation
    ]

main :: IO ()
main = do
  mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]

  Counts {cases, tried, errors, failures} <- runTestTT $ TestList tests
  when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure

