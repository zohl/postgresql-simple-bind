{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}

-- Legend:
--   A CRUD-like API for table of users.
--   Due to backward compatibility requirements some of legacy types must be
--   preserved.
--
-- This is an example of using custom types.


module ExUsers (
    specUsers
  ) where

import Common (bindOptions, include)
import Control.Applicative((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, char, takeWhile1, scan, sepBy)
import Data.Text (Text)
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Bind (bindFunction, PostgresType)
import Database.PostgreSQL.Simple.Bind.Types()
import Database.PostgreSQL.Simple.FromField (Conversion(..), FromField(..), ResultError(..), typename, returnError)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC8
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)

concat <$> mapM (bindFunction bindOptions) [
    "function get_users(p_filter varchar2 default '') returns setof t_user"
  , "function add_user(p_name varchar2, p_age bigint) returns bigint"
  , "function del_user(p_user_id bigint) returns void"
  ]

type instance PostgresType "varchar2" = Text


data User = User {
    userId   :: Int
  , userName :: String
  , userAge  :: Int
  } deriving (Eq, Show)

type instance PostgresType "t_user" = User


-- TODO switch to FromField class
class FromField1 a where
  fromField1 :: BS.ByteString -> Conversion a

instance FromField1 Int where
  fromField1 = return . read . BSC8.unpack

instance FromField1 String where
  fromField1 = return . BSC8.unpack


instance FromField User where
  fromField f v = (("t_user" /=) <$> typename f) >>= \case
    True  -> returnError Incompatible f ""
    False -> ($ v) $ maybe
      (returnError UnexpectedNull f "")
      (either (returnError ConversionFailed f) id
       . (parseOnly value)) where

         value :: Parser (Conversion User)
         value = do
           [Just userId, Just userName, Just userAge] <- row
           return $ User <$> (fromField1 userId) <*> (fromField1 userName) <*> (fromField1 userAge)

         row :: Parser [Maybe BS.ByteString]
         row = (char '(') *> (fld `sepBy` (char ',')) <* (char ')')

         fld :: Parser (Maybe BS.ByteString)
         fld = (Just <$> quotedString) <|> (Just <$> unquotedString) <|> (pure Nothing) where
           quotedString = unescape <$> (char '"' *> scan False updateState) where
             updateState isBalanced c = if
               | c == '"'             -> Just . not $ isBalanced
               | not isBalanced       -> Just False
               | c == ',' || c == ')' -> Nothing
               | otherwise            -> fail $ "Unexpected symbol: " ++ [c]

             unescape = halve '\\' b0 . groupByChar '\\'
                      . halve '"' b0  . groupByChar '"'
                      . BSC8.init where

               b0 = byteString BS.empty

               groupByChar c = BSC8.groupBy $ \a b -> (a == c) == (b == c)

               halve :: Char -> Builder -> [BS.ByteString] -> BS.ByteString
               halve c b []     = BSL.toStrict . toLazyByteString $ b
               halve c b (s:ss) = halve c (b <> b') ss where
                 b' = if
                   | (/= c) . BSC8.head $ s -> byteString s
                   | otherwise              -> byteString . BS.take ((BS.length s) `div` 2) $ s

           unquotedString = takeWhile1 (\c -> c /= ',' && c /= ')')



specUsers :: Connection -> Spec
specUsers conn = describe "Users example" $ it "works" $ do
  include conn "./examples/sql/users.sql"

  mrFooId <- sqlAddUser conn "Mr. Foo" 42
  mrBarId <- sqlAddUser conn "Mr. Bar" 53
  mrBazId <- sqlAddUser conn "Mr. Baz" 64

  sqlGetUsers conn (Just "Mr. Foo") >>= \[u] -> (u { userId = 1 }) `shouldBe` (User {
      userId   = 1
    , userName = "Mr. Foo"
    , userAge  = 42
    })

  sqlGetUsers conn Nothing >>= shouldBe [mrFooId, mrBarId, mrBazId] . map userId
  sqlGetUsers conn (Just "Mr. Ba_") >>= shouldBe [mrBarId, mrBazId] . map userId
  sqlDelUser conn mrBarId

  sqlGetUsers conn Nothing >>= shouldBe [mrFooId, mrBazId] . map userId

