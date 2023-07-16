module Minimalisp.Control.Parser.Text.FoldCharsSpec where

import Harness.ParserCase
import Test.Hspec
import Data.Char
import Minimalisp.Control.Parser.Text.FoldChars

spec :: Spec
spec = do
  let digitGe500 s (Just c)
        | not . isDigit $ c = Left "not a digit"
        | s * 10 + digitToInt c < 500 = Right . Left $ s * 10 + digitToInt c
        | otherwise = Right . Right $ c
      digitGe500 _ Nothing = Left "EOF"
  parserCase
    (foldChars digitGe500 0)
    "foldChars digitGe500 0"
    $ do
      "1009" `shouldParseTo` '9'
      "501" `shouldParseTo` '1'
      "50169" `shouldParseTo` '1' `withRemainder` "69"
      "40169" `shouldParseTo` '6' `withRemainder` "9"
      
      "10abc" `shouldFailWithReason` "not a digit" `andRemainder` "abc"
      "10" `shouldFailWithReason` "EOF" `andRemainder` ""
