module Minimalisp.Control.Parser.Combinator.WithErrorReasonSpec where

import Test.Hspec
import Harness.ParserCase
import Minimalisp.Control.Parser.Text.CharEq
import Minimalisp.Control.Parser.Combinator.WithErrorReason

spec :: Spec
spec = do
  parserCase
    (charEq 'a' \@/ "test error msg")
    "charEq 'a' \\@/ \"test error msg\""
    $ do
      "a" `shouldParseTo` 'a'
      "abc" `shouldParseTo` 'a' `withRemainder` "bc"
      "b" `shouldFailWithReason` "test error msg" `andRemainder` "b"
      "" `shouldFailWithReason` "test error msg" `andRemainder` ""
