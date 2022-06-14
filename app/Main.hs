module Main where

import TelegramBot
import BotState
import Control.Monad.State
import Data.HashMap

main :: IO ()
main = evalStateT botState (WikiBotState 0 empty)
