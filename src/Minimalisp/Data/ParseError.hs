{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Minimalisp.Data.ParseError where

data ParseError = ParseError
  { reason :: String,
    currentInput :: String
  }
  deriving (Show, Eq)

setCurrentInput :: String -> ParseError -> ParseError
setCurrentInput s pe = pe { currentInput = s }

setReason :: String -> ParseError -> ParseError
setReason s pe = pe { reason = s }
