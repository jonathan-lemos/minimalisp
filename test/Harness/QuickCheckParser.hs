module Harness.QuickCheckParser where

import Minimalisp.Control.Parser
import Minimalisp.Data.ParseError
import Test.Hspec
import Test.Hspec.QuickCheck

quickCheckParserValue :: (Show a, Eq a) => Parser a -> String -> (String -> Either ParseError (String, a)) -> SpecWith ()
quickCheckParserValue a title f =
  prop title $
    \x -> parse a x `shouldBe` f x

quickCheckParserSatisfies :: (Show a) => Parser a -> String -> (String -> Either ParseError (String, a) -> Bool) -> SpecWith ()
quickCheckParserSatisfies a title f =
  prop title $
    \x -> parse a x `shouldSatisfy` f x
