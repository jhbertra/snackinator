{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Snackinator
    ( snackMain
    ) where

import Control.Monad
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Casing
import Data.Bifunctor
import Debug.Trace
import GHC.Generics (Generic)
import Network.HTTP.Req

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BU
import qualified Data.ByteString as B


data Gender
    = Male
    | Female
    | NonBinary
    deriving (Eq, Ord, Show, Generic)

data Snacker = Snacker
    { snackerId :: Int
    , snackerFirstName :: String
    , snackerLastName :: String
    , snackerEmail :: String
    , snackerGender :: Gender
    , snackerIpAddress :: String
    , snackerFaveSnack :: String
    }
    deriving (Eq, Ord, Show, Generic)

data Variant = Variant
    { variantId :: Int
    , variantPrice :: String
    }
    deriving (Eq, Ord, Show, Generic)

data Product = Product
    { productId :: Int
    , productTitle :: String
    , productTags :: [String]
    , productVariants :: [Variant]
    }
    deriving (Eq, Ord, Show, Generic)

newtype Products = Products { products :: [Product] } deriving (Eq, Ord, Show, Generic)

instance FromJSON Gender
instance FromJSON Snacker where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance FromJSON Variant where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance FromJSON Product where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance FromJSON Products

snackMain :: IO ()
snackMain = do
    p <- runExceptT $ do
        snackers <- getSnackers
        products <- getProducts
        pure products
    print p

getSnackers :: ExceptT String IO [Snacker]
getSnackers = getJsonDoc (https "s3.amazonaws.com" /: "misc-file-snack" /: "MOCK_SNACKER_DATA.json") mempty

getProducts :: ExceptT String IO Products
getProducts = getJsonDoc (https "ca.desknibbles.com" /: "products.json") $
    "limit" =: (250 :: Int)
    <> header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:68.0) Gecko/20100101 Firefox/68.0"

getJsonDoc :: FromJSON a => Url Https -> Option Https -> ExceptT String IO a
getJsonDoc url opts = ExceptT
    . fmap (eitherDecode . (\a -> trace (show a) a) . responseBody)
    . runReq defaultHttpConfig
    $ req GET url NoReqBody lbsResponse opts