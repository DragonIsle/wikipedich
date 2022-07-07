{-# LANGUAGE DeriveGeneric #-}

module Locales where

import Data.HashMap as HM (Map, fromList)
import Data.Hashable
import GHC.Generics (Generic)

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

