{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TgMessages where

import Control.Monad
import GHC.Generics (Generic)
import Data.Aeson.Types
import Network.HTTP.Simple
import Data.Foldable (toList)

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


-- | Parse response with JSON into a list of updates (TgUpdate) or return parsing error 
parseUpdatesResponse :: Response Value -> Either String [TgUpdate]
parseUpdatesResponse resp =
  parseEither
    ( withObject "TgUpdate" $
        withArray "TgUpdate" (traverse parseJSON . toList) <=< (.: "result")
    )
    (getResponseBody resp)
    
parseMessagesResponse :: Response Value -> Either String TgMessage
parseMessagesResponse resp = parseEither parseJSON (getResponseBody resp)