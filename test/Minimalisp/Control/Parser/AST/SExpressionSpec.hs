module Minimalisp.Control.Parser.AST.SExpressionSpec where

import Harness.ParserCase
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.AST.SExpression
import Minimalisp.Data.AST.SExpression
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  modifyMaxSuccess (const 1000) $
    prop "number quickCheck" $
      \n -> parse number (show n) `shouldBe` Right ("", Number (read . show $ (n :: Double)))

  parserCase
    number
    "number"
    $ do
      "123" `shouldParseTo` Number 123
      "+123" `shouldParseTo` Number 123
      "-123" `shouldParseTo` Number (-123)
      "123." `shouldParseTo` Number 123 `withRemainder` "."
      "123.a" `shouldParseTo` Number 123 `withRemainder` ".a"
      "123e" `shouldParseTo` Number 123 `withRemainder` "e"
      "123ea" `shouldParseTo` Number 123 `withRemainder` "ea"
      " 123" `shouldParseTo` Number 123
      " 123 456" `shouldParseTo` Number 123 `withRemainder` " 456"
      "123.45" `shouldParseTo` Number 123.45
      "-123.45" `shouldParseTo` Number (-123.45)
      "2.4e-10" `shouldParseTo` Number 2.4e-10
      "2.4e+10" `shouldParseTo` Number 2.4e10
      "2.4e10" `shouldParseTo` Number 2.4e10
      "-2.4e10" `shouldParseTo` Number (-2.4e10)
      "24e-10" `shouldParseTo` Number 24e-10
      "24e+10" `shouldParseTo` Number 24e10
      "24e10" `shouldParseTo` Number 24e10
      "-24e10" `shouldParseTo` Number (-24e10)

      "a1" `shouldFailWithReason` "Expected digits, got 'a'." `andRemainder` "a1"
      "-a1" `shouldFailWithReason` "Expected digits, got 'a'." `andRemainder` "a1"
      "+a1" `shouldFailWithReason` "Expected digits, got 'a'." `andRemainder` "a1"
      "" `shouldFailWithReason` "Expected digits, got EOF." `andRemainder` ""

  prop "character quickCheck" $
    \c -> parse character (show c) `shouldBe` Right ("", Character c)

  parserCase
    character
    "character"
    $ do
      "'a'" `shouldParseTo` Character 'a'
      "'\\0'" `shouldParseTo` Character '\0'
      " 'a'" `shouldParseTo` Character 'a'
      " 'a' bc" `shouldParseTo` Character 'a' `withRemainder` " bc"

      "" `shouldFailWithReason` "Reached EOF before reading closing \'." `andRemainder` ""
      "'a foo bar" `shouldFailWithReason` "Reached EOF before reading closing \'." `andRemainder` ""

  parserCase
    boolean
    "boolean"
    $ do
      "true" `shouldParseTo` Boolean True
      "false" `shouldParseTo` Boolean False
      " true" `shouldParseTo` Boolean True
      " true bar" `shouldParseTo` Boolean True `withRemainder` " bar"

      "" `shouldFailWithReason` "Expected \"true\" or \"false\", got EOF." `andRemainder` ""
      "tru" `shouldFailWithReason` "Expected \"true\" or \"false\", got \"tru\"." `andRemainder` "tru"
      "oof" `shouldFailWithReason` "Expected \"true\" or \"false\", got \"oof\"." `andRemainder` "oof"

  parserCase
    nil
    "nil"
    $ do
      "nil" `shouldParseTo` Nil
      " nil" `shouldParseTo` Nil
      " nil bar" `shouldParseTo` Nil `withRemainder` " bar"

      "" `shouldFailWithReason` "Expected 'n', but got EOF." `andRemainder` ""
      "nii" `shouldFailWithReason` "Expected 'l', but got 'i'." `andRemainder` "i"
      "foo" `shouldFailWithReason` "Expected 'n', but got 'f'." `andRemainder` "foo"

  parserCase
    identifier
    "identifier"
    $ do
      "abc" `shouldParseTo` Identifier "abc"
      "abc def" `shouldParseTo` Identifier "abc" `withRemainder` " def"
      " abc def" `shouldParseTo` Identifier "abc" `withRemainder` " def"
      "a9" `shouldParseTo` Identifier "a9"
      "a_9" `shouldParseTo` Identifier "a_9"
      "a-9" `shouldParseTo` Identifier "a-9"
      "9" `shouldFailWithReason` "Expected a-zA-Z, but got '9'" `andRemainder` "9"
      "" `shouldFailWithReason` "Expected a-zA-Z, but got EOF" `andRemainder` ""
      "9 foo" `shouldFailWithReason` "Expected a-zA-Z, but got '9'" `andRemainder` "9 foo"

  parserCase
    functionCallInner
    "functionCallInner"
    $ do
      "a b c" `shouldParseTo` FunctionCall "a" [Identifier "b", Identifier "c"]
