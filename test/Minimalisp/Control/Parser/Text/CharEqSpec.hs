module Minimalisp.Control.Parser.Text.CharEqSpec where

import Test.Hspec
import Harness.ParserCase
import Minimalisp.Control.Parser.Text.CharEq

spec :: Spec
spec = do
  parserCase
    (charEq 'a')
    "charEq 'a'"
    $ do
      "a" `shouldParseTo` 'a'
      "abc" `shouldParseTo` 'a' `withRemainder` "bc"
      "d" `shouldFailWithReason` "Expected 'a', but got 'd'." `andRemainder` "d"
      "def" `shouldFailWithReason` "Expected 'a', but got 'd'." `andRemainder` "def"
      "" `shouldFailWithReason` "Expected 'a', but got EOF." `andRemainder` ""
