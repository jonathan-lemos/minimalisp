{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Harness.ParserCase where
import Harness.Collector
import Test.Hspec
import Minimalisp.Control.Parser
import Minimalisp.Data.ParseError

data ValueExpectation a = ShouldEqual a | ShouldSatisfy (a -> Bool) | ShouldFail String

data ParserCaseLine a = ParserCaseLine
  { pcInput :: String,
    pcExpectation :: ValueExpectation a,
    pcRemainder :: String
  }

withRemainder :: (Show a, Eq a) => ParserCase a b -> String -> ParserCase a b
withRemainder pc remainder =
  let modifyPcl pcl = pcl { pcRemainder = remainder }
  in mutateLast modifyPcl pc

infix 3 `withRemainder`

type ParserCase a b = Collector (ParserCaseLine a) b

shouldParseTo :: (Show a, Eq a) => String -> a -> ParserCase a ()
shouldParseTo input result = liftCollector ParserCaseLine {pcInput = input, pcExpectation = ShouldEqual result, pcRemainder = ""}

shouldParseAndSatisfy :: (Show a, Eq a) => String -> (a -> Bool) -> ParserCase a ()
shouldParseAndSatisfy input predicate = liftCollector ParserCaseLine {pcInput = input, pcExpectation = ShouldSatisfy predicate, pcRemainder = ""}

shouldFailWithReason :: (Show a, Eq a) => String -> String -> String -> ParserCase a ()
shouldFailWithReason input rsn remainder = liftCollector ParserCaseLine {pcInput = input, pcExpectation = ShouldFail rsn, pcRemainder = remainder}

andRemainder :: (Show a, Eq a) => (String -> ParserCase a b) -> String -> ParserCase a b
andRemainder = ($)

infix 3 `andRemainder`

pcLineToExpectation :: (Show a, Eq a) => ParserCaseLine a -> Parser a -> Expectation
pcLineToExpectation (ParserCaseLine input expectation remainder) parser =
  let parseResult = parse parser input
  in case expectation of
      ShouldEqual expected -> parseResult `shouldBe` Right (remainder, expected)
      ShouldSatisfy predicate -> do
        case parseResult of
          Left pe -> expectationFailure (show pe)
          Right (ci, v) -> do
            ci `shouldBe` remainder
            v `shouldSatisfy` predicate
      ShouldFail rsn -> parseResult `shouldBe` Left (ParseError {reason = rsn, currentInput = remainder})

parserCase :: (Show a, Eq a) => Parser a -> String -> ParserCase a b -> SpecWith ()
parserCase parser title pc =
  it title $ mapM_ (($ parser) . pcLineToExpectation) (collectedValues pc)

