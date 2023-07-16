module Minimalisp.Control.Parser.Text.UtilsSpec where

import Test.Hspec
import Minimalisp.Control.Parser.Text.Utils
import Harness.FunctionCase
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  functionCase diagnosticFirstChar "diagnosticFirstChar" $ do
    "abc" `shouldEvalTo` "'a'"
    "a" `shouldEvalTo` "'a'"
    "" `shouldEvalTo` "EOF"

  functionCase quote "quote" $ do
    "abc" `shouldEvalTo` "\"abc\""
    "a" `shouldEvalTo` "\"a\""
    "" `shouldEvalTo` "\"\""

  describe "trimToLength" $ do
    functionCase (trimToLength 3) "length 3" $ do
      "abc" `shouldEvalTo` "abc"
      "" `shouldEvalTo` ""
      "abcdef" `shouldEvalTo` "abc"
      "abcdefg" `shouldEvalTo` "abc"
      "abcdefghi" `shouldEvalTo` "abc"

    functionCase (trimToLength 6) "length 6" $ do
      "abc" `shouldEvalTo` "abc"
      "" `shouldEvalTo` ""
      "abcdef" `shouldEvalTo` "abcdef"
      "abcdefg" `shouldEvalTo` "abc..."
      "abcdefghi" `shouldEvalTo` "abc..."

    prop "stays under given length" $
      \x -> length (trimToLength 8 x) <= 8

  functionCase diagnosticToken "diagnosticToken" $ do
    "abc def" `shouldEvalTo` "\"abc\""
    "  abc def" `shouldEvalTo` "whitespace"
    "  " `shouldEvalTo` "whitespace"
    "" `shouldEvalTo` "EOF"
