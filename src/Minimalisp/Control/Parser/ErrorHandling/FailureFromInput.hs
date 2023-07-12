module Minimalisp.Control.Parser.ErrorHandling.FailureFromInput where

import Control.Applicative
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput

failureFromInput :: (String -> String) -> Parser a
failureFromInput = (empty \@/$)

