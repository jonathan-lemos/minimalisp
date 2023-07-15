module Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInputSpec where

import Harness.ParserCase
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser.Text.CharEq
import Test.Hspec

spec :: Spec
spec = do
  parserCase
    (charEq 'a' \@/$ ("Error: " <>))
    "charEq 'a' \\@/$ (\"Error: \" <>)"
    $ do
      "a" `shouldParseTo` 'a'
      "abc" `shouldParseTo` 'a' `withRemainder` "bc"
      "" `shouldFailWithReason` "Error: " `andRemainder` ""
      "bcd" `shouldFailWithReason` "Error: bcd" `andRemainder` "bcd"
