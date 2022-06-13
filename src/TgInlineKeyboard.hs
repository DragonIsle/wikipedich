{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TgInlineKeyboard (InlineKeyboardButton (..)) where

import GHC.Generics (Generic)
import Data.Aeson.Types

-- | Button that attached to a message in tg chat
data InlineKeyboardButton = InlineKeyboardButton
  { text :: String,
    url :: Maybe String,
    callback_data :: Maybe String
  } deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton tx (Just u) (Just cd)) = object ["text" .= tx, "url" .= u, "callback_data" .= cd]
  toJSON (InlineKeyboardButton tx (Just u) _) = object ["text" .= tx, "url" .= u]
  toJSON (InlineKeyboardButton tx _ (Just cd)) = object ["text" .= tx, "callback_data" .= cd]
  toJSON (InlineKeyboardButton tx _ _) = object ["text" .= tx]
