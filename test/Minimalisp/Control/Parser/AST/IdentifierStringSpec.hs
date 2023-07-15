module Minimalisp.Control.Parser.AST.IdentifierStringSpec where
import Test.Hspec
import Harness.ParserCase
import Minimalisp.Control.Parser.AST.IdentifierString

spec :: Spec
spec = do
  parserCase
    identifierString
    "identifierString"
    $ do
      "abc" `shouldParseTo` "abc"
      "abc def" `shouldParseTo` "abc" `withRemainder` " def"
      " abc def" `shouldParseTo` "abc" `withRemainder` " def"
      "a9" `shouldParseTo` "a9"
      "a_9" `shouldParseTo` "a_9"
      "a-9" `shouldParseTo` "a-9"
      "9" `shouldFailWithReason` "Expected a-zA-Z, but got '9'" `andRemainder` "9"
      "" `shouldFailWithReason` "Expected a-zA-Z, but got EOF" `andRemainder` ""
      "9 foo" `shouldFailWithReason` "Expected a-zA-Z, but got '9'" `andRemainder` "9 foo"
