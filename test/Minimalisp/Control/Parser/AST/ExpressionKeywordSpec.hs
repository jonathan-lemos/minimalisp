module Minimalisp.Control.Parser.AST.ExpressionKeywordSpec where

import Harness.ParserCase
import Minimalisp.Control.Parser.AST.ExpressionKeyword
import Minimalisp.Data.AST.ExpressionKeyword
import Test.Hspec

spec :: Spec
spec = do
  parserCase
    expressionKeyword
    "expressionKeyword"
    $ do
      "function" `shouldParseTo` Function
      "  function" `shouldParseTo` Function
      "  function foo" `shouldParseTo` Function `withRemainder` " foo"

      "case" `shouldParseTo` Case
      "  case" `shouldParseTo` Case
      "  case foo" `shouldParseTo` Case `withRemainder` " foo"

      "" `shouldFailWithReason` "Expected \"case\" or \"function\", got EOF." `andRemainder` ""
      "aaa" `shouldFailWithReason` "Expected \"case\" or \"function\", got \"aaa\"." `andRemainder` "aaa"
      "aaa bbb" `shouldFailWithReason` "Expected \"case\" or \"function\", got \"aaa\"." `andRemainder` "aaa bbb"
