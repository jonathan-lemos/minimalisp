module Minimalisp.Control.Parser.Text.WhitespaceSpec where

import Harness.ParserCase
import Minimalisp.Control.Parser.Text.Whitespace
import Test.Hspec

spec :: Spec
spec = do
  parserCase
    whitespace
    "whitespace"
    $ do
      "   " `shouldParseTo` "   "
      "   abc" `shouldParseTo` "   " `withRemainder` "abc"
      "a   123abc" `shouldParseTo` "" `withRemainder` "a   123abc"
