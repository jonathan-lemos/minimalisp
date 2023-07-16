module Minimalisp.Control.Parser.Text.StringWhileSpec where
import Test.Hspec
import Harness.ParserCase
import Data.Char
import Minimalisp.Control.Parser.Text.StringWhile

spec :: Spec
spec = do
  parserCase
    (stringWhile isDigit)
    "stringWhile isDigit"
    $ do
      "123" `shouldParseTo` "123"
      "123abc" `shouldParseTo` "123" `withRemainder` "abc"
      "a123abc" `shouldParseTo` "" `withRemainder` "a123abc"

