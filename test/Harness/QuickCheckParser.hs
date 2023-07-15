module Harness.QuickCheckParser (quickCheckParser) where

import Minimalisp.Control.Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Minimalisp.Data.ParseError

quickCheckParser :: (Show a, Eq a) => Parser a -> String -> (String -> Either ParseError (String, a)) -> SpecWith ()
quickCheckParser a title f =
  prop title $
    \x -> parse a x `shouldBe` f x
