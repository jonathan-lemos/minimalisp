{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Monad law, right identity" #-}
{-# HLINT ignore "Use >=>" #-}
module Minimalisp.Control.ParserSpec where

import Data.Char
import Data.List (singleton)
import Harness.ParserCase
import Harness.VerifyParserEquality
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Text.Char
import Minimalisp.Control.Parser.Text.CharAny
import Minimalisp.Control.Parser.Text.StringAny
import Test.Hspec
import Harness.QuickCheckParser
import Minimalisp.Data.ParseError
import Control.Applicative

spec :: Spec
spec = do
  describe "Parser" $ do
    let ad = charAny "abcd"
    let aw = charAny "abcdefghijklmnopqrstuvw"
    describe "Functor" $ do
      parserCase (singleton <$> ad) "fmap basic functionality" $ do
        "a" `shouldParseTo` "a"
        "abc" `shouldParseTo` "a" `withRemainder` "bc"
        "efg" `shouldFailWithReason` "Expected any of \"abcd\", but got 'e'." `andRemainder` "efg"
        "" `shouldFailWithReason` "Expected any of \"abcd\", but got EOF." `andRemainder` ""

      quickCheckParserEquality
        (id <$> aw)
        aw
        "fmap id == id"

      quickCheckParserEquality
        ((+ 1) . ord <$> aw)
        ((+ 1) <$> ord <$> aw)
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
          "xq" `shouldFailWithReason` "Expected any of [\"a\",\"bc\"], but got \"xq\"." `andRemainder` "xq"
          "xq bar" `shouldFailWithReason` "Expected any of [\"a\",\"bc\"], but got \"xq\"." `andRemainder` "xq bar"
          "aq" `shouldFailWithReason` "Expected any of [\"d\",\"efg\"], but got \"q\"." `andRemainder` "q"
          "aq bar" `shouldFailWithReason` "Expected any of [\"d\",\"efg\"], but got \"q\"." `andRemainder` "q bar"

      parserCase
        (pure singleton <*> ad)
        "pure basic functionality"
        $ do
          "a" `shouldParseTo` "a"
          "abc" `shouldParseTo` "a" `withRemainder` "bc"
          "efg" `shouldFailWithReason` "Expected any of \"abcd\", but got 'e'." `andRemainder` "efg"
          "" `shouldFailWithReason` "Expected any of \"abcd\", but got EOF." `andRemainder` ""

      quickCheckParserEquality
        (pure (+ 1) <*> pure (4 :: Int))
        (pure ((+ 1) 4))
        "pure f <*> pure a == pure (f a)"

      quickCheckParserEquality
        (pure (+ 1) <*> pure (4 :: Int))
        (pure ($ 4) <*> pure (+ 1))
        "u <*> pure y == pure ($ y) <*> u"

      quickCheckParserEquality
        (pure (.) <*> pure show <*> pure (+ 1) <*> pure (4 :: Int))
        (pure show <*> (pure (+ 1) <*> pure (4 :: Int)))
        "pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"

    describe "Monad" $ do
      let cord = ord <$> char
      let add v = (+ v) <$> cord
      let mult n = (* n) <$> cord

      parserCase
        ( do
            c <- ad
            c2 <- ad
            return [c, c2]
        )
        ">>= basic functionality"
        $ do
          "ad" `shouldParseTo` "ad"
          "abc" `shouldParseTo` "ab" `withRemainder` "c"
          "" `shouldFailWithReason` "Expected any of \"abcd\", but got EOF." `andRemainder` ""
          "a" `shouldFailWithReason` "Expected any of \"abcd\", but got EOF." `andRemainder` ""
          "aec" `shouldFailWithReason` "Expected any of \"abcd\", but got 'e'." `andRemainder` "ec"

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

    describe "MonadFail" $ do
      quickCheckParserValue
        (fail "foo bar" :: Parser Char)
        "fail \"foo bar\""
        (\x -> Left (ParseError { reason = "foo bar", currentInput = x }))

    describe "Semigroup" $ do
      parserCase
        (stringAny ["a", "bc"] <> stringAny ["d", "efg"])
        "stringAny [\"a\", \"bc\"] <> stringAny [\"d\", \"efg\"]"
        $ do
          "ad" `shouldParseTo` "ad"
          "bcd" `shouldParseTo` "bcd"
          "aefg" `shouldParseTo` "aefg"
          "xq" `shouldFailWithReason` "Expected any of [\"a\",\"bc\"], but got \"xq\"." `andRemainder` "xq"
          "xq bar" `shouldFailWithReason` "Expected any of [\"a\",\"bc\"], but got \"xq\"." `andRemainder` "xq bar"
          "aq" `shouldFailWithReason` "Expected any of [\"d\",\"efg\"], but got \"q\"." `andRemainder` "q"
          "aq bar" `shouldFailWithReason` "Expected any of [\"d\",\"efg\"], but got \"q\"." `andRemainder` "q bar"

    describe "Monoid" $ do
      quickCheckParserValue
        (mempty :: Parser String)
        "mempty (String)"
        (Right . (, mempty))

    describe "Alternative" $ do
      parserCase
        (stringAny ["a", "bcdm", "bcxy"] <|> stringAny ["d", "bcdmn", "bcef"])
        "stringAny [\"a\", \"bcd\"] <|> stringAny [\"d\", \"bde\"]"
        $ do
          "a" `shouldParseTo` "a"
          "d" `shouldParseTo` "d"
          "bcef" `shouldParseTo` "bcef"
          "bcxy" `shouldParseTo` "bcxy"

          "bcdmn" `shouldParseTo` "bcdm" `withRemainder` "n"

          "aq" `shouldParseTo` "a" `withRemainder` "q"
          "dq" `shouldParseTo` "d" `withRemainder` "q"
          "bcefq" `shouldParseTo` "bcef" `withRemainder` "q"
          "bcxyq" `shouldParseTo` "bcxy" `withRemainder` "q"

          "bc" `shouldFailWithReason` "Expected any of [\"dmn\",\"ef\"], but got EOF." `andRemainder` ""
          "bcx" `shouldFailWithReason` "Expected \"y\", but got EOF." `andRemainder` ""
          "bce" `shouldFailWithReason` "Expected \"f\", but got EOF." `andRemainder` ""
