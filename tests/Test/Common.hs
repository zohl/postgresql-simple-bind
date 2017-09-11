{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Test.Common (
    PGSql(..)

  , proxyArbitrary
  , proxyMap

  , arbitraryString

  , CharClass(..)
  , inClass
  , charASCII
  , charASCIInl
  , charOperator
  , charId
  , charTag

  , arbitrarySumDecomposition
  ) where

import Data.Char (chr)
import Data.Proxy (Proxy(..))
import Control.Monad (liftM2)
import Test.QuickCheck (Gen, Arbitrary(..), sized, oneof, choose)
import GHC.TypeLits (Symbol)


class PGSql a where
  render :: a -> String


proxyArbitrary :: (Arbitrary a) => Proxy a -> Gen a
proxyArbitrary _ = arbitrary

proxyMap :: Proxy (f :: Symbol -> *) -> Proxy a -> Proxy (f a)
proxyMap _ _ = Proxy


data CharClass
  = Uniform        String
  | RestrictedHead String String

ccHead :: CharClass -> Gen Char
ccHead (Uniform s)          = inClass s
ccHead (RestrictedHead s _) = inClass s

ccTail :: CharClass -> Gen Char
ccTail (Uniform s)           = inClass s
ccTail (RestrictedHead s s') = inClass (s ++ s')


arbitraryString :: CharClass -> Gen String
arbitraryString c = sized $ \case
  0 -> return []
  1 -> pure <$> (ccHead c)
  n -> liftM2 (:) (ccHead c) (sequence . replicate (n-1) $ ccTail c)


inClass :: String -> Gen Char
inClass = oneof . inClass' where
  inClass' (x:'-':y:xs) = (choose (x, y)):(inClass' xs)
  inClass' (x:xs)       = (return x):(inClass' xs)
  inClass' []           = []


charASCII :: CharClass
charASCII = Uniform [chr 32, '-', chr 127]

charASCIInl :: CharClass
charASCIInl = Uniform [chr 32, '-', chr 127, '\n']

charTag :: CharClass
charTag = RestrictedHead "A-Za-z_" "0-9"

charId :: CharClass
charId = RestrictedHead "A-Za-z_" "0-9$"

charOperator :: CharClass
charOperator = Uniform "+*/<>=~!@#%^&|`?-"


arbitrarySumDecomposition :: Int -> Gen [Int]
arbitrarySumDecomposition 0 = return []
arbitrarySumDecomposition n = choose (1, n) >>= \k -> (k:) <$> (arbitrarySumDecomposition (n-k))
