module Main where

import TelegramBot
import Control.Monad.State

main :: IO ()
main = evalStateT botState (0 :: Int)
