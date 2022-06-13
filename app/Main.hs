module Main where

import TelegramBot
import BotState
import Control.Monad.State
import Data.HashMap

main :: IO ()
main = evalStateT botState (WikiBotState { userLocales = empty, offset = 0, userCallbackMessages = empty })
