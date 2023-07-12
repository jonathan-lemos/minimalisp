module Minimalisp.Control.Parser.Text.Utils where

import Data.Char
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Text.StringWhile

diagnosticFirstChar :: String -> String
diagnosticFirstChar = \case
  (x : _) -> show x
  [] -> "EOF"

quote :: String -> String
quote s = "\"" <> s <> "\""

trimToLength :: Int -> String -> String
trimToLength len s
  | length s <= len = s
  | len <= 3 = take 3 s
  | otherwise = take (len - 3) s <> "..."

diagnosticToken :: String -> String
diagnosticToken ci =
  (quote . trimToLength 15)
    (either (const "EOF") snd
      (parse (stringWhile (not . isSpace)) ci))
    
