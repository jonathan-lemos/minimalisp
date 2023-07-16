{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Harness.FunctionCase where
import Harness.Collector
import Test.Hspec

data ValueExpectation a = ShouldEqual a | ShouldSatisfy (a -> Bool)

data FunctionCaseLine a b = FunctionCaseLine
  { fcInput :: a,
    fcExpectation :: ValueExpectation b}

type FunctionCase a b c = Collector (FunctionCaseLine a b) c

shouldEvalTo :: (Show b, Eq b) => a -> b -> FunctionCase a b ()
shouldEvalTo input result = liftCollector FunctionCaseLine {fcInput = input, fcExpectation = ShouldEqual result}

shouldEvalAndSatisfy :: (Show b, Eq b) => a -> (b -> Bool) -> FunctionCase a b ()
shouldEvalAndSatisfy input predicate = liftCollector FunctionCaseLine {fcInput = input, fcExpectation = ShouldSatisfy predicate}

fcLineToExpectation :: (Show b, Eq b) => FunctionCaseLine a b -> (a -> b) -> Expectation
fcLineToExpectation (FunctionCaseLine input expectation) f =
  case expectation of
      ShouldEqual expected -> f input `shouldBe` expected
      ShouldSatisfy predicate -> f input `shouldSatisfy` predicate

functionCase :: (Show b, Eq b) => (a -> b) -> String -> FunctionCase a b c -> SpecWith ()
functionCase parser title fc =
  it title $ mapM_ (($ parser) . fcLineToExpectation) (collectedValues fc)

