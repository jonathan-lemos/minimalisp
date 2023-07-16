module Minimalisp.Control.Parser.Text.StringAnySpec where

import Harness.ParserCase
import Minimalisp.Control.Parser.Text.StringAny
import Test.Hspec

spec :: Spec
spec = do
  parserCase
    (stringAny ["abc", "abd", "xyz"])
    "stringAny [\"abc\", \"abd\", \"xyz\"]"
    $ do
      "abc" `shouldParseTo` "abc"
      "abd" `shouldParseTo` "abd"
      "xyz" `shouldParseTo` "xyz"

      "abe" `shouldFailWithReason` "Expected any of [\"c\",\"d\"], but got \"e\"." `andRemainder` "e"
      "xy" `shouldFailWithReason` "Expected \"z\", but got EOF." `andRemainder` ""
      "qrs" `shouldFailWithReason` "Expected any of [\"abc\",\"abd\",\"xyz\"], but got \"qrs\"." `andRemainder` "qrs"
