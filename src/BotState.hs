{-# LANGUAGE DeriveGeneric #-}

module BotState where

import Data.HashMap as HM (Map, fromList)
import Data.Hashable
import GHC.Generics (Generic)
import TgInlineKeyboard (InlineKeyboardButton)

data Locale = UK | EN | RU deriving (Enum, Eq, Ord, Generic)

instance Hashable Locale

data StandardMessages = StandardMessages { noInfoFound :: String, seeInWiki :: String }

defaultLocale :: Locale
defaultLocale = EN

instance Show Locale where
  show l = case l of
    UK -> "uk"
    RU -> "ru"
    _ -> show defaultLocale
    
defaultLocaleMessages :: StandardMessages
defaultLocaleMessages = StandardMessages "Nothing found" "See in Wikipedia"

localeMap :: HM.Map Locale StandardMessages
localeMap =
  HM.fromList [ (UK, StandardMessages "Нічого не знайдено" "Подивитись у Вікіпедіі"),
    (RU, StandardMessages "Ничего не найдено" "Посмотреть в Википедии"),
    (EN, defaultLocaleMessages)
  ]
  
data WikiBotState = WikiBotState { 
  userLocales :: HM.Map Int Locale,
  offset :: Int,
  userCallbackMessages :: HM.Map String (String, [[InlineKeyboardButton]])
}

