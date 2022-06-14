{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TgMessages where

import Control.Monad
import GHC.Generics (Generic)
import Data.Aeson.Types
import Network.HTTP.Simple
import Data.Foldable (toList)
import Data.Maybe (isJust, maybeToList, fromMaybe)

-- | Telegram Chat
data TgChat = TgChat { username :: String, chat_id :: Int } deriving (Show, Generic)

instance FromJSON TgChat where
  parseJSON = withObject "TgChat" $ \v -> TgChat <$> v .: "username" <*> v .: "id"
   
-- | Message from someone that was sent to the bot
data TgMessage = TgMessage
  { text :: Maybe String,
    message_id :: Int,
    chat :: TgChat
  } deriving (Show, Generic, FromJSON)

-- | Callback query from a callback button
data CallbackQuery = CallbackQuery
  { cq_id :: String,
    user_id :: Int,
    cq_data :: Maybe String,
    cq_message :: Maybe TgMessage
  } deriving (Show, Generic)
  
instance FromJSON CallbackQuery where
  parseJSON = withObject "CallbackQuery" $ \v -> CallbackQuery
    <$> v .: "id"
    <*> ((v .: "from") >>= (.: "id"))
    <*> v .:? "data"
    <*> v .:? "message"
    
-- | Update from tg side with message or/and callbackQuery
data TgUpdate = TgUpdate
  { update_id :: Int,
    upd_message :: Maybe TgMessage,
    callback_query :: Maybe CallbackQuery
  } deriving (Show, Generic)

instance FromJSON TgUpdate where
  parseJSON = withObject "TgUpdate" $ \v -> TgUpdate
    <$> v .: "update_id"
    <*> v .:? "message"
    <*> v .:? "callback_query"

data GroupedTgUpdates = GroupedTgUpdates { textMessagesFromChats :: [TgMessage], buttonPressCallbacks :: [CallbackQuery] }

-- | Extracts tg chat id and message that was sent from TgMessage
extractChatIdAndText :: TgMessage -> (Int, String)
extractChatIdAndText msg = (chat_id $ chat msg, fromMaybe "" $ text msg)

-- | Groups different tg updates by their logical type
groupTgUpdates :: [TgUpdate] -> GroupedTgUpdates
groupTgUpdates updates = let 
    messagesWithText = filter (isJust . text) $ (maybeToList . upd_message) =<< updates
    buttonCallbacks = filter (isJust . cq_data) $ (maybeToList . callback_query) =<< updates
  in GroupedTgUpdates messagesWithText buttonCallbacks
  
-- | Parse response with JSON into a list of updates (TgUpdate) or return parsing error 
parseUpdatesResponse :: Response Value -> Either String [TgUpdate]
parseUpdatesResponse resp =
  parseEither
    ( withObject "TgUpdate" $
        withArray "TgUpdate" (traverse parseJSON . toList) <=< (.: "result")
    )
    (getResponseBody resp)

-- | Parse response with JSON into a list of updates (TgUpdate) or return parsing error
parseMessagesResponse :: Response Value -> Either String TgMessage
parseMessagesResponse resp = parseEither parseJSON (getResponseBody resp)