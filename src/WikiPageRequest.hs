{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module WikiPageRequest (WikiPageInfo, searchPages, extract, url) where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Text as T (pack)
import Control.Monad
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as S8
import Data.List (intercalate)


data WikiPageInfo = WikiPageInfo
  { title :: String
  , pageid :: Int
  , extract :: String
  }
  deriving (Show, Generic)
  
url :: String -> WikiPageInfo -> String
url lang pageInfo = "https://" ++ lang ++ ".wikipedia.org/wiki" ++ "?curid=" ++ show (pageid pageInfo)

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


searchPages :: String -> [Int] -> IO [WikiPageInfo]
searchPages lang pageIds = do
  emptyPageReq <- parseRequest $ "GET https://" ++ lang ++ ".wikipedia.org/w/api.php"

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