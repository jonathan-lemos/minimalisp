module Minimalisp.Control.Parser.Combinator.MaybeParseSpec where

import Data.Either
import Harness.ParserCase
import Harness.QuickCheckParser
import Minimalisp.Control.Parser.Combinator.MaybeParse
import Minimalisp.Control.Parser.Text.CharEq
import Minimalisp.Control.Parser.Text.StringEq
import Test.Hspec

spec :: Spec
spec = do
  parserCase
    (maybeParse $ stringEq "ab")
    "maybeParse $ stringEq \"ab\""
    $ do
      "ab" `shouldParseTo` Just "ab" `withRemainder` ""
      "abc" `shouldParseTo` Just "ab" `withRemainder` "c"

      "" `shouldParseTo` Nothing
      "acd" `shouldParseTo` Nothing `withRemainder` "acd"

  quickCheckParserSatisfies
    (maybeParse $ charEq 'a')
    "maybeParse never fails"
    (\_ x -> isRight x)
