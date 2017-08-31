{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Test.Common (
    PGSql(..)

  , proxyArbitrary
  , proxyMap

  , arbitraryString
  , arbitraryString'

  , inClass
  , charASCII
  , charOperator
  , charId'
  , charTag'

  , arbitrarySumDecomposition
  ) where

import Data.Char (chr)
import Data.Proxy (Proxy(..))
import Control.Monad (liftM2)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, choose)
import GHC.TypeLits (Symbol)


class PGSql a where
  render :: a -> String


proxyArbitrary :: (Arbitrary a) => Proxy a -> Gen a
proxyArbitrary _ = arbitrary

proxyMap :: Proxy (f :: Symbol -> *) -> Proxy a -> Proxy (f a)
proxyMap _ _ = Proxy


arbitraryString :: Gen Char -> Gen String
arbitraryString c = sized $ \n -> sequence . replicate n $ c

arbitraryString' :: (Gen Char, Gen Char) -> Gen String
arbitraryString' (c, c') = sized $ \case
  0 -> return []
  1 -> pure <$> c
  n -> liftM2 (:) c (resize (n-1) $ arbitraryString c')


inClass :: String -> Gen Char
inClass = oneof . inClass' where
  inClass' (x:'-':y:xs) = (choose (x, y)):(inClass' xs)
  inClass' (x:xs)       = (return x):(inClass' xs)
  inClass' []           = []

charASCII :: Gen Char
charASCII = inClass [chr 32, '-', chr 127]

charOperator :: Gen Char
charOperator = inClass "+*/<>=~!@#%^&|`?-"

charId' :: (Gen Char, Gen Char)
charId' = (inClass "A-Za-z_", inClass "A-Za-z0-9_$")

charTag' :: (Gen Char, Gen Char)
charTag' = (inClass "A-Za-z_", inClass "A-Za-z0-9_")


arbitrarySumDecomposition :: Int -> Gen [Int]
arbitrarySumDecomposition 0 = return []
arbitrarySumDecomposition n = choose (1, n) >>= \k -> (k:) <$> (arbitrarySumDecomposition (n-k))
