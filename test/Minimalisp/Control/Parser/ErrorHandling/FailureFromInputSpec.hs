module Minimalisp.Control.Parser.ErrorHandling.FailureFromInputSpec where

import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.ErrorHandling.FailureFromInput
import Test.Hspec
import Harness.QuickCheckParser (quickCheckParser)
import Minimalisp.Data.ParseError

spec :: Spec
spec = do
  quickCheckParser
    (failureFromInput ("Error: " <>) :: Parser Char)
    "failureFromInput (\"Error: \" <>)"
    (\x -> Left (ParseError { reason = "Error: " <> x, currentInput = x }))
