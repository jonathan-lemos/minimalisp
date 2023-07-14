module Minimalisp.Control.ParserSpec where

import Test.Hspec
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Text.CharAny
import Data.List (singleton)
import Minimalisp.Data.ParseError

spec :: Spec
spec = parallel $ do
  describe "parser functor" $ do
    it "fmaps correctly" $ do
      let f = parse $ singleton <$> charAny "abcd"
      f "a" `shouldBe` Right ("", "a")
      f "abc" `shouldBe` Right ("bc", "a")
      f "efg" `shouldBe` Left (ParseError {currentInput = "efg", reason = "Expected any of \"abcd\", but got 'e'"})
