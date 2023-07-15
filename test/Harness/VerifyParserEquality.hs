module Harness.VerifyParserEquality (verifyParserEquality, quickCheckParserEquality) where

import Minimalisp.Control.Parser
import Test.Hspec
import Test.Hspec.QuickCheck

makeCase :: (Show a, Eq a) => String -> Parser a -> Parser a -> Expectation
makeCase v a b = parse a v `shouldBe` parse b v

verifyParserEquality :: (Show a, Eq a) => Parser a -> Parser a -> String -> [String] -> SpecWith ()
verifyParserEquality a b title cases =
  it title $ mapM_ (\c -> c a b) (makeCase <$> cases)

quickCheckParserEquality :: (Show a, Eq a) => Parser a -> Parser a -> String -> SpecWith ()
quickCheckParserEquality a b title =
  prop title $
    \x -> parse a x `shouldBe` parse b x
