{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use <$>" #-}
module Minimalisp.Control.ParserSpec where

import Data.List (singleton)
import Harness.ParserCase
import Harness.VerifyParserEquality
import Minimalisp.Control.Parser.Text.CharAny
import Minimalisp.Control.Parser.Text.StringAny
import Test.Hspec
import Data.Char

spec :: Spec
spec = parallel $ do
  describe "Parser" $ do
    let ad = charAny "abcd"
    describe "Functor" $ do
      parserCase (singleton <$> ad) "fmap basic functionality" $ do
        "a" `shouldParseTo` "a"
        "abc" `shouldParseTo` "a" `withRemainder` "bc"
        "efg" `shouldFailWithReason` "Expected any of \"abcd\", but got 'e'" `andRemainder` "efg"
        "" `shouldFailWithReason` "Expected any of \"abcd\", but got EOF" `andRemainder` ""

      verifyParserEquality
        (id <$> ad)
        ad
        "fmap id == id"
        [ "a",
          "abc",
          "efg",
          ""
        ]

      verifyParserEquality
        ((+1) . ord <$> ad)
        ((+1) <$> ord <$> ad)
        "fmap (f . g) == fmap f . fmap g"
        [ "a",
          "abc",
          "efg",
          ""
        ]

    describe "Applicative" $ do
      parserCase
        ( (\a b -> length $ a <> b)
            <$> stringAny ["a", "bc"]
            <*> stringAny ["d", "efg"]
        )
        "<*> basic functionality"
        $ do
          "ad" `shouldParseTo` 2
          "bcd" `shouldParseTo` 3
          "aefg" `shouldParseTo` 4
          "aq" `shouldFailWithReason` "Expected any of [\"d\",\"efg\"], but got \"q\"" `andRemainder` "q"

      parserCase
        (pure singleton <*> ad)
        "pure basic functionality"
        $ do
          "a" `shouldParseTo` "a"
          "abc" `shouldParseTo` "a" `withRemainder` "bc"
          "efg" `shouldFailWithReason` "Expected any of \"abcd\", but got 'e'" `andRemainder` "efg"
          "" `shouldFailWithReason` "Expected any of \"abcd\", but got EOF" `andRemainder` ""

      verifyParserEquality
        (pure (+1) <*> pure (4 :: Int))
        (pure ((+1) 4))
        "pure f <*> pure a == pure (f a)"
        ["a", "abc", ""]

      verifyParserEquality
        (pure (+1) <*> pure (4 :: Int))
        (pure ($ 4) <*> pure (+1))
        "u <*> pure y == pure ($ y) <*> u"
        ["a", "abc", ""]

      verifyParserEquality
        (pure (.) <*> pure show <*> pure (+1) <*> pure (4 :: Int))
        (pure show <*> (pure (+1) <*> pure (4 :: Int)))
        "pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"
        ["a", "abc", ""]
