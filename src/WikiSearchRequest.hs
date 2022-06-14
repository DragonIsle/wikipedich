{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WikiSearchRequest (searchUnits, WikiSearchUnit(..)) where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Control.Monad
import Network.HTTP.Types (queryTextToQuery)
import Data.Text (pack)
import BotState (Locale)
  
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

-- | Search passed string in the local wikipedia (indentified by locale passed)
searchUnits :: Locale -> String -> IO [WikiSearchUnit]
searchUnits locale str = do
  emptySearchReq <- parseRequest $ "GET https://" ++ show locale ++ ".wikipedia.org/w/api.php"
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