{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Minimalisp.Data.ParseError where

data ParseError = ParseError
  { reason :: String,
    currentInput :: String
  }
  deriving (Show, Eq)

newReasonFromCurrentInput :: (String -> String) -> ParseError -> ParseError
newReasonFromCurrentInput f pe = pe { reason = f $ currentInput pe }

setReason :: String -> ParseError -> ParseError
setReason = newReasonFromCurrentInput . const

