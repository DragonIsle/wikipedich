{-# LANGUAGE OverloadedStrings #-}

module TelegramBot (botState) where

import TgMessageSender
import qualified WikiPageRequest as WP
import qualified WikiSearchRequest as WS
import Data.Maybe (fromMaybe, isJust, maybeToList)
import qualified TgMessages as TM
import qualified TgInlineKeyboard as TI
import Control.Monad.State
import BotState
import qualified Data.HashMap as HM
import Data.UUID.V4 (nextRandom)


makeInlineKeyboard :: String -> [String] -> String -> [[TI.InlineKeyboardButton]]
makeInlineKeyboard seeInWikiText uuids url =
  [
    [TI.InlineKeyboardButton {
      TI.text = seeInWikiText,
      TI.url = Just url,
      TI.callback_data = Nothing
    } ],
    (\(idx, uuid) -> TI.InlineKeyboardButton {
      TI.text = show idx,
      TI.url = Nothing,
      TI.callback_data = Just uuid
    }) <$> zip [1..] uuids
  ]
searchInWikiForUpdate :: Locale -> String -> Int -> IO (HM.Map String (String, [[TI.InlineKeyboardButton]]))
searchInWikiForUpdate locale text chatId = do
  units <- WS.searchUnits (show locale) text
  pages <- WP.searchPages (show locale) (WS.pageid <$> units)
  let lcMessages = fromMaybe defaultLocaleMessages $ HM.lookup locale localeMap
  let pagesWithExtract = filter (not . null . WP.extract) pages
  let msg = if null pagesWithExtract then
              noInfoFound lcMessages
              else WP.extract $ head pagesWithExtract
  uuids <- traverse (const nextRandom) pagesWithExtract
  let strUuids = show <$> uuids
  let makeKeyBoardByUrl = makeInlineKeyboard (seeInWiki lcMessages) strUuids
  let keyboardButtons = makeKeyBoardByUrl (WP.url (show locale) (head pagesWithExtract))
  sendMessageWithInlineKeyboard keyboardButtons chatId msg
  return $ HM.fromList (zip strUuids ((\page -> (WP.extract page, makeKeyBoardByUrl (WP.url (show locale) page))) <$> pagesWithExtract))

botState :: StateT WikiBotState IO ()
botState = do
  currentOffset <- gets offset
  botUpdates <- lift $ getUpdatesWithOffset currentOffset
  let newMessages = filter (isJust . TM.text) $ (maybeToList . TM.upd_message) =<< botUpdates
  let msgTextsWithChatIds = (\msg -> (fromMaybe "" $ TM.text msg, TM.chat_id $ TM.chat msg)) <$> newMessages
  let newCallbacks = filter (isJust . TM.cq_data) $ (maybeToList . TM.callback_query) =<< botUpdates
  callbackMsgs <- gets userCallbackMessages
  _ <- lift $ traverse (\callback ->
      let maybe_cq_message = TM.cq_message callback
          chatId = maybe 0 (TM.chat_id . TM.chat) maybe_cq_message
          msgId = maybe 0 TM.message_id maybe_cq_message
          cqData = fromMaybe "" (TM.cq_data callback)
          cbMsg = HM.findWithDefault ("", []) cqData callbackMsgs
       in editMessageWithKeyboard chatId msgId (snd cbMsg) (fst cbMsg)
    ) newCallbacks
--  locales <- gets userLocales
  -- todo: change UK to user locale
  pagesByChatId <- lift $ traverse (uncurry $ searchInWikiForUpdate UK) msgTextsWithChatIds
  withStateT (\oldS -> oldS {
      userCallbackMessages = foldl HM.union (userCallbackMessages oldS) pagesByChatId,
      offset = foldl max 0 (TM.update_id <$> botUpdates) + 1
    }) botState


