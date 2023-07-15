module Minimalisp.Control.Parser.Combinator.ParenthesizeSpec where

import Harness.ParserCase
import Test.Hspec
import Minimalisp.Control.Parser.Combinator.Parenthesize
import Minimalisp.Control.Parser.Text.CharEq

spec :: Spec
spec = do
  parserCase
    (parenthesize (charEq 'a'))
    "parenthesize (charEq 'a')"
    $ do
      "(a)" `shouldParseTo` 'a'
      " (a )" `shouldParseTo` 'a'
      "(b)" `shouldFailWithReason` "Expected 'a', but got 'b'." `andRemainder` "b)"
      "a)" `shouldFailWithReason` "Expected '(', but got 'a'." `andRemainder` "a)"
      "(a" `shouldFailWithReason` "Expected ')', but got EOF." `andRemainder` ""
      "(ab" `shouldFailWithReason` "Expected ')', but got 'b'." `andRemainder` "b"
