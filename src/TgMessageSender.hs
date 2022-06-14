{-# LANGUAGE OverloadedStrings #-}

module TgMessageSender where

import Data.Aeson.Types
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Client.Conduit (responseTimeoutMicro)
import Network.HTTP.Simple
import qualified TgMessages as TM
import qualified TgInlineKeyboard as TI
import Data.Either (isLeft, fromLeft)
import Control.Monad (when)

-- | TG bot token (SHOULD BE MOVED TO CONFIG)
token :: String
token = "5333507327:AAHu5pMZ43pLFD7DtLYHFStEP20te67Mx2o"

-- | URL for TG API
baseUrl :: String
baseUrl = "https://api.telegram.org/bot" ++ token ++ "/"

-- | Query new updates with given offset (offset required to avoid updates double-fetching)
getUpdatesWithOffset :: Int -> IO [TM.TgUpdate]
getUpdatesWithOffset offset = do
  emptyUpdatesReq <- parseRequest $ baseUrl ++ "getUpdates"

  let updatesReq =
        setRequestQueryString
          [ ("offset", Just $ (S8.pack . show) offset),
            ("timeout", Just "30")
          ]
          $ setRequestSecure True $
            setRequestResponseTimeout
              (responseTimeoutMicro 31000000)
              emptyUpdatesReq
  updatesResp <- httpJSON updatesReq
  let parseResult = TM.parseUpdatesResponse updatesResp
  when (isLeft parseResult) $ print $ "Error while parsing TgUpdate list from response: " ++ fromLeft "" parseResult
  return $ concat parseResult

-- | Edit already sent message (identified by chat_id and msgId) and inline keyboard attached to it
editMessageWithKeyboard :: Int -> Int -> [[TI.InlineKeyboardButton]] -> String -> IO ()
editMessageWithKeyboard chat_id msgId keyboard msg = do
  emptyEditMessageReq <- parseRequest $ baseUrl ++ "editMessageText"

  _ <- (httpJSON $ setRequestBodyJSON (
      object [ 
        ("chat_id", toJSON chat_id),
        ("message_id", toJSON msgId),
        ("text", toJSON msg), 
        ("reply_markup", object [("inline_keyboard", toJSON keyboard)]) 
      ]
    ) $ setRequestHeader "Content-Type" ["application/json"]
      $ setRequestSecure True emptyEditMessageReq) :: IO (Response Value)
  return ()
--  print $ "Telegram editMessageText query response: " ++ show (getResponseBody resp :: Value)


-- | Send text message to given chat (by it's Int id) with additional body params provided (list of Pairs)
sendMessageWithParams :: [Pair] -> Int -> String -> IO ()
sendMessageWithParams params chatId msg = do
  emptySendMessageReq <- parseRequest $ baseUrl ++ "sendMessage"

  _ <- (httpJSON $ setRequestBodyJSON (
      object $ [ ("chat_id", toJSON chatId), ("text", toJSON msg) ] ++ params
    ) $ setRequestHeader "Content-Type" ["application/json"]
      $ setRequestSecure True emptySendMessageReq) :: IO (Response Value)
  return ()
--  print $ "Telegram sendMessage query response: " ++ show (getResponseBody sendMessageResp :: Value)

-- | Send simple text message
sendSimpleMessage :: Int -> String -> IO ()
sendSimpleMessage = sendMessageWithParams []

-- | Send text message with a inline keyboard buttons attached to it
sendMessageWithInlineKeyboard :: [[TI.InlineKeyboardButton]] -> Int -> String -> IO ()
sendMessageWithInlineKeyboard keyboard = sendMessageWithParams [
    ("reply_markup", object [("inline_keyboard", toJSON keyboard)])
  ]

