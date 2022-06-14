{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module WikiPageRequest (WikiPageInfo (..), searchPages, url) where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Text as T (pack)
import Control.Monad
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as S8
import Data.List (intercalate)
import BotState (Locale)


data WikiPageInfo = WikiPageInfo
  { title :: String
  , pageid :: Int
  , extract :: String
  }
  deriving (Show, Generic)
  
-- | makes wikipedia page url for locale and pageId
url :: Locale -> Int -> String
url locale pageId = "https://" ++ show locale ++ ".wikipedia.org/wiki" ++ "?curid=" ++ show pageId

instance ToJSON WikiPageInfo where
  toJSON (WikiPageInfo t pid ex) = object [(T.pack . show) pid .=
      object ["title" .= t, "pageid" .= pid, "extract" .= ex]
    ]

parseWikiPageInfo :: Value -> Parser WikiPageInfo
parseWikiPageInfo = withObject "WikiPageInfo" $ \v -> WikiPageInfo
  <$> v .: "title"
  <*> v .: "pageid"
  <*> v .: "extract"

parsePageResponse :: Response Value -> [Int] -> Maybe [WikiPageInfo]
parsePageResponse resp = traverse (\pid -> parseMaybe (withObject "WikiPageInfo" $
    parseWikiPageInfo <=< (.: (T.pack . show) pid) <=< (.: "pages") <=< (.: "query")
  ) (getResponseBody resp))


-- | search pages in local wiki (identified by locale) for passed pageid list
searchPages :: Locale -> [Int] -> IO [WikiPageInfo]
searchPages locale pageIds = do
  emptyPageReq <- parseRequest $ "GET https://" ++ show locale ++ ".wikipedia.org/w/api.php"

  let pageReq = setRequestQueryString [
                      ("action", Just "query"),
                      ("format", Just "json"),
                      ("prop", Just "extracts"),
                      ("explaintext", Just "1"),
                      ("exlimit", Just "20"),
                      ("exintro", Just "1"),
                      ("pageids", Just $ (S8.pack . intercalate "|") (show <$> pageIds))
                    ] $ setRequestSecure True emptyPageReq
  pageResp <- httpJSON pageReq
  return $ concat (parsePageResponse pageResp pageIds)