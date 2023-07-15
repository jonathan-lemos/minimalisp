{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Monad law, right identity" #-}
{-# HLINT ignore "Use >=>" #-}
module Minimalisp.Control.ParserSpec where

import Data.List (singleton)
import Harness.ParserCase
import Harness.VerifyParserEquality
import Minimalisp.Control.Parser.Text.CharAny
import Minimalisp.Control.Parser.Text.StringAny
import Test.Hspec
import Data.Char
import Minimalisp.Control.Parser.Text.Char

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

      quickCheckParserEquality
        (id <$> ad)
        ad
        "fmap id == id"

      quickCheckParserEquality
        ((+1) . ord <$> ad)
        ((+1) <$> ord <$> ad)
        "fmap (f . g) == fmap f . fmap g"

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

      quickCheckParserEquality
        (pure (+1) <*> pure (4 :: Int))
        (pure ((+1) 4))
        "pure f <*> pure a == pure (f a)"

      quickCheckParserEquality
        (pure (+1) <*> pure (4 :: Int))
        (pure ($ 4) <*> pure (+1))
        "u <*> pure y == pure ($ y) <*> u"

      quickCheckParserEquality
        (pure (.) <*> pure show <*> pure (+1) <*> pure (4 :: Int))
        (pure show <*> (pure (+1) <*> pure (4 :: Int)))
        "pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"

    describe "Monad" $ do
      let cord = ord <$> char
      let add v = (+ v) <$> cord
      let mult n = (*n) <$> cord

      parserCase
        (do
          c <- ad
          c2 <- ad
          return [c, c2])
        ">>= basic functionality"
        $ do
          "ad" `shouldParseTo` "ad"
          "abc" `shouldParseTo` "ab" `withRemainder` "c"
          "" `shouldFailWithReason` "Expected any of \"abcd\", but got EOF" `andRemainder` ""
          "a" `shouldFailWithReason` "Expected any of \"abcd\", but got EOF" `andRemainder` ""
          "aec" `shouldFailWithReason` "Expected any of \"abcd\", but got 'e'" `andRemainder` "ec"
      
      quickCheckParserEquality
        (return 5 >>= add)
        (add 5)
        "return a >>= h == h a"

      quickCheckParserEquality
        (cord >>= return)
        cord
        "m >>= return == m"

      quickCheckParserEquality
        (cord >>= add >>= mult)
        (cord >>= (\x -> add x >>= mult))
        "(m >>= g) >>= h == m >>= (\\x -> g x >>= h)"

