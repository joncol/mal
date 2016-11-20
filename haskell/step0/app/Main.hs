module Main where

import System.Console.Haskeline
import System.IO

import Mal

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "user> "
      case minput of
        Nothing    -> return ()
        Just input -> do
          outputStrLn $ rep input
          loop
