{-# LANGUAGE DeriveGeneric #-}

module Snackinator
    ( snackMain
    ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Casing
import Data.Bifunctor
import GHC.Generics (Generic)
import Network.HTTP
import Network.Stream
import Network.URI (parseURI)

import qualified Data.ByteString.Lazy as B


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


instance FromJSON Gender
instance FromJSON Snacker where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

snackMain :: IO ()
snackMain = print =<< (getJsonDoc snackerUrl :: IO (Either String [Snacker]))

snackerUrl :: String
snackerUrl = "http://s3.amazonaws.com/misc-file-snack/MOCK_SNACKER_DATA.json"

getJsonDoc :: FromJSON a => String -> IO (Either String a)
getJsonDoc = fmap (eitherDecode . rspBody <=< first show) . simpleHTTP . getBsRequest

getBsRequest :: String -> Request B.ByteString
getBsRequest urlString =
    case parseURI urlString of
      Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
      Just u  -> mkRequest GET u
  