module Main where

import           Data.Bool (bool)
import           Data.List.NonEmpty hiding (head, map)
import           Data.Maybe (listToMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           System.Console.Haskeline
import           Text.Megaparsec

import           Mal

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "user> "
      case minput of
        Nothing    -> return ()
        Just ""    -> loop
        Just input -> do
          case rep input of
            Left e  -> outputStrLn $ formatError e
            Right r -> outputStrLn r
          loop

formatError :: ShowErrorComponent e => ParseError Char e -> String
formatError e@(ParseError _ got expect _) = expString ++ ", " ++ gotString
  where
    expString = case getClosingToken expect of
                  Nothing -> "expected " ++ parseErrorPretty e
                  Just ct -> "expected '" ++ [ct] ++ "'"
    gotString = "got EOF" -- TODO: Don't hardcode this

getClosingToken :: Set (ErrorItem Char) -> Maybe Char
getClosingToken ts =
  listToMaybe $ foldr accum [] ")]"
  where
    makeTokens c  = Tokens $ c :| []
    accum c = bool id ((:) c) $ S.member (makeTokens c) ts
