{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module DatabaseService (saveCallbacks, selectCallbackByCqData) where

import Database.SQLite.Simple
import TgInlineKeyboard (InlineKeyboardButton (..))
import Data.UUID.V4 (nextRandom)
import Data.HashMap as HM (Map, toList, fromList)
import Data.Foldable (traverse_)
import Data.List (groupBy, sortBy)

withConn :: String -> (Connection -> IO a) -> IO a
withConn dbName action = do
   conn <- open dbName
   res <- action conn
   close conn
   return res

insertCallbackMessage :: String -> String -> Maybe String -> IO ()
insertCallbackMessage cq_data message maybeKeyboardId = withConn "wikibot.db" $ \conn -> do
  execute conn 
    "INSERT INTO callback_messages \
     \ (cq_data, message, inline_keyboard_id) VALUES (?, ?, ?)" 
    (cq_data, message, maybeKeyboardId)
  print "callback_message added"


keyboardToButtonRows :: String -> [[InlineKeyboardButton]] -> [(String, Int, Int, String, Maybe String, Maybe String)]
keyboardToButtonRows keyboardId keyboard = do
  (buttonRow, rowId) <- keyboard `zip` [1..]
  (btn, buttonId) <- buttonRow `zip` [1..]
  return (keyboardId, rowId, buttonId, text btn, url btn, callback_data btn)

insertKeyboardButtons :: String -> [[InlineKeyboardButton]] -> IO ()
insertKeyboardButtons keyboardId keyboard = withConn "wikibot.db" $ \conn -> do
  executeMany conn
    "INSERT INTO keyboard_buttons \
     \(keyboard_id, row_order, button_order, text, url, callback_data) \
     \ VALUES (?, ?, ?, ?, ?, ?)"
    $ keyboardToButtonRows keyboardId keyboard
  print "keyboard buttons added"


saveCallbacks :: HM.Map String (String, [[InlineKeyboardButton]]) -> IO ()
saveCallbacks cmap = traverse_ (\(cq_data, (message, keyboard)) -> do 
    keyboardId <- show <$> nextRandom
    insertCallbackMessage cq_data message (Just keyboardId)
    insertKeyboardButtons keyboardId keyboard
  ) $ toList cmap



data CallbackButtonRow = CallbackButtonRow { 
  cqData :: String,
  msg :: String,
  rNum :: Int,
  bNum :: Int,
  button :: InlineKeyboardButton } deriving Show

instance FromRow CallbackButtonRow where
  fromRow = CallbackButtonRow <$> field
                                <*> field
                                <*> field
                                <*> field
                                <*> (InlineKeyboardButton <$> field <*> field <*> field)


buttonTuplesToCallbacksMap :: [CallbackButtonRow] -> HM.Map String (String, [[InlineKeyboardButton]])
buttonTuplesToCallbacksMap buttons = fromList $ do
  cqButtons <- groupBy (\cb1 cb2-> cqData cb1 == cqData cb2) buttons
  let cm = (\cb -> (cqData cb, msg cb)) . head $ cqButtons
  let rowGroupped = groupBy (\cb1 cb2 -> rNum cb1 == rNum cb2) $ sortBy (\cb1 cb2 -> compare (rNum cb1) (rNum cb2)) cqButtons
  let buttonRows = (button <$>) . sortBy (\cb1 cb2 -> compare (bNum cb1) (bNum cb2)) <$> rowGroupped
  return (fst cm, (snd cm, buttonRows))


selectCallbackByCqData :: IO (HM.Map String (String, [[InlineKeyboardButton]]))
selectCallbackByCqData = buttonTuplesToCallbacksMap <$> withConn "wikibot.db" (\conn ->
  query_ conn
   "select cq_data, message, row_order, button_order, text, url, callback_data \
    \ from callback_messages \
    \ inner join keyboard_buttons kb on kb.keyboard_id = callback_messages.inline_keyboard_id" 
    :: IO [CallbackButtonRow])

