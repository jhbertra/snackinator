{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Snackinator
    ( snackMain
    ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Casing
import Data.Bifunctor
import Data.List
import Data.Maybe
import Debug.Trace
import GHC.Generics (Generic)
import Network.HTTP.Req
import Text.Read

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BU
import qualified Data.ByteString as B

snackMain :: IO ()
snackMain = do
    result <- runExceptT $ do
        snackers <- getSnackers
        products <- getProducts
        pure
            $ map ((fst . head) &&& map snd)
            $ groupBy
                (\x y -> productId (fst x) == productId (fst y))
                [(p, s) | p <- products,
                          s <- snackers,
                          snackerFaveSnack s == productTitle p ]
    case result of
        Left e -> error e
        Right snackerSnacks -> do
            putStrLn ""
            putStrLn "Real snacks found:"
            forM_ snackerSnacks $ \(product, snackers) ->
                putStrLn $
                    "  "
                    ++ productTitle product
                    ++ " (fave snack of: "
                    ++ intercalate ", " (map snackerEmail snackers)
                    ++ ")"
            putStrLn ""
            let snacksFlat = concatMap (\(product, snackers) -> map (const product) snackers) snackerSnacks
            putStrLn $
                "If all "
                ++ show (length snacksFlat)
                ++ " snackers bought their fave snack, in total it would cost $"
                ++ show (sum $ mapMaybe (readMaybe :: String -> Maybe Double) $ concatMap (map variantPrice . productVariants) snacksFlat)

getSnackers :: ExceptT String IO [Snacker]
getSnackers = getJsonDoc (https "s3.amazonaws.com" /: "misc-file-snack" /: "MOCK_SNACKER_DATA.json") mempty

getProducts :: ExceptT String IO [Product]
getProducts = fmap products $ getJsonDoc (https "ca.desknibbles.com" /: "products.json") $
    "limit" =: (250 :: Int)
    <> header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:68.0) Gecko/20100101 Firefox/68.0"

getJsonDoc :: FromJSON a => Url Https -> Option Https -> ExceptT String IO a
getJsonDoc url opts = ExceptT
    . fmap (eitherDecode . responseBody)
    . runReq defaultHttpConfig
    $ req GET url NoReqBody lbsResponse opts

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
