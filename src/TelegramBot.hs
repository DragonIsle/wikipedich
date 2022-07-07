{-# LANGUAGE OverloadedStrings #-}

module TelegramBot (botState) where

import TgMessageSender
import qualified WikiPageRequest as WP
import qualified WikiSearchRequest as WS
import Data.Maybe (fromMaybe)
import qualified TgMessages as TM
import qualified TgInlineKeyboard as TI
import Control.Monad.State
import Locales
import qualified Data.HashMap as HM
import Data.UUID.V4 (nextRandom)
import TgMessages (groupTgUpdates, textMessagesFromChats, buttonPressCallbacks, extractChatIdAndText)
import DatabaseService

makeInlineKeyboard :: String -> [String] -> String -> [[TI.InlineKeyboardButton]]
makeInlineKeyboard seeInWikiText uuids url =
  [
    [TI.InlineKeyboardButton seeInWikiText (Just url) Nothing],
    (\(idx, uuid) -> TI.InlineKeyboardButton (show idx) Nothing (Just uuid)) <$> zip [1..] uuids
  ]

searchWikiPages :: Locale -> String -> IO [WP.WikiPageInfo]
searchWikiPages locale text = do
  units <- WS.searchUnits locale text
  pages <- WP.searchPages locale (WS.pageid <$> units)
  return $ filter (not . null . WP.extract) pages

type CallbacksWithKeyboardData = HM.Map String (String, [[TI.InlineKeyboardButton]])


-- | Searching passed text in wiki (identified by locale) and send result
-- | to the user: first page (most popular) showed already, other ones 
-- | available through buttons in inline keyboard
searchInWikiAndSendResult :: Locale -> Int -> String -> IO CallbacksWithKeyboardData
searchInWikiAndSendResult locale chatId text = do
  pages <- searchWikiPages locale text
  strCallbackUuids <- traverse (const $ show <$> nextRandom) pages
  let lcMessages = fromMaybe defaultLocaleMessages $ HM.lookup locale localeMap
  let makeKeyboardFromPage = makeInlineKeyboard (seeInWiki lcMessages) strCallbackUuids . WP.url locale . WP.pageid
  if null pages then sendSimpleMessage chatId $ noInfoFound lcMessages
    else sendMessageWithInlineKeyboard (makeKeyboardFromPage $ head pages) chatId (WP.extract $ head pages)
  return $ HM.fromList (zip strCallbackUuids ((\page -> (WP.extract page, makeKeyboardFromPage page)) <$> pages))


-- | Main function that recursively fetch telegram updates,
-- | do required actions for each update type and update state with new info
botState :: StateT Int IO ()
botState = do
  currentOffset <- get
  botUpdates <- lift $ getUpdatesWithOffset currentOffset
  let groupedUpdates = groupTgUpdates botUpdates
  callbackMsgs <- lift selectCallbackByCqData
  _ <- lift $ traverse (\callback ->
      let maybe_cq_message = TM.cq_message callback
          chatId = maybe 0 (TM.chat_id . TM.chat) maybe_cq_message
          msgId = maybe 0 TM.message_id maybe_cq_message
          cqData = fromMaybe "" (TM.cq_data callback)
          cbMsg = HM.findWithDefault ("", []) cqData callbackMsgs
       in editMessageWithKeyboard chatId msgId (snd cbMsg) (fst cbMsg)
    ) $ buttonPressCallbacks groupedUpdates
  let chatIdsWithMessageTexts = extractChatIdAndText <$> textMessagesFromChats groupedUpdates
    -- todo: change UK to user locale
  pagesByChatId <- lift $ traverse (uncurry $ searchInWikiAndSendResult UK) chatIdsWithMessageTexts
  lift $ saveCallbacks $ foldl HM.union HM.empty pagesByChatId
  put $ foldl max 0 (TM.update_id <$> botUpdates) + 1
  withStateT (\_ -> foldl max 0 (TM.update_id <$> botUpdates) + 1) botState


