{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WikiSearchRequest (searchUnits, WikiSearchUnit, pageid) where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Control.Monad
import Network.HTTP.Types (queryTextToQuery)
import Data.Text (pack)
  
data WikiSearchUnit = WikiSearchUnit
  { wordcount :: Int
  , size :: Int
  , pageid :: Int
  , snippet :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

parseSearchResponse :: Response Value -> Maybe [WikiSearchUnit]
parseSearchResponse resp = parseMaybe (withObject "WikiSearchUnit" $
    parseJSON <=< (.: "search") <=< (.: "query")
  ) (getResponseBody resp)

searchUnits :: String -> String -> IO [WikiSearchUnit]
searchUnits lang str = do
  emptySearchReq <- parseRequest $ "GET https://" ++ lang ++ ".wikipedia.org/w/api.php"
  let searchReq = setRequestQueryString (queryTextToQuery [
                        ("action", Just "query"),
                        ("format", Just "json"),
                        ("utf8", Just "1"),
                        ("list", Just "search"),
                        ("srsearch", Just $ Data.Text.pack str)
                      ])
                      $ setRequestSecure True emptySearchReq
  searchResp <- httpJSON searchReq
  print $ parseSearchResponse searchResp
  return $ concat $ parseSearchResponse searchResp