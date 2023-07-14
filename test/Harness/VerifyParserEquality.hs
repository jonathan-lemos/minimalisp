module Harness.VerifyParserEquality (verifyParserEquality) where

import Minimalisp.Control.Parser
import Test.Hspec

makeCase :: (Show a, Eq a) => String -> Parser a -> Parser a -> Expectation
makeCase v a b = parse a v `shouldBe` parse b v

verifyParserEquality :: (Show a, Eq a) => Parser a -> Parser a -> String -> [String] -> SpecWith ()
verifyParserEquality a b title cases =
  it title $ mapM_ (\c -> c a b) (makeCase <$> cases)
